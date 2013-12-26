package kvstore

import scala.concurrent.duration.DurationInt

import Persistence.Persist
import Replicator.Replicate
import akka.actor.Actor
import akka.actor.PoisonPill
import akka.actor.ActorRef
import akka.actor.Cancellable
import akka.actor.Props
import akka.actor.actorRef2Scala
import kvstore.Arbiter.Join
import kvstore.Arbiter.JoinedPrimary
import kvstore.Arbiter.JoinedSecondary
import kvstore.Arbiter.Replicas
import scala.language.postfixOps

object Replica {
  sealed trait Operation {
    def key: String
    def id: Long
  }
  case class Insert(key: String, value: String, id: Long) extends Operation
  case class Remove(key: String, id: Long) extends Operation
  case class Get(key: String, id: Long) extends Operation

  sealed trait OperationReply
  case class OperationAck(id: Long) extends OperationReply
  case class OperationFailed(id: Long) extends OperationReply
  case class GetResult(key: String, valueOption: Option[String], id: Long) extends OperationReply

  def props(arbiter: ActorRef, persistenceProps: Props): Props = Props(new Replica(arbiter, persistenceProps))
}

class Replica(val arbiter: ActorRef, persistenceProps: Props) extends Actor {
  import Replica._
  import Replicator._
  import Persistence._
  import context.dispatcher
  import akka.actor.UntypedActor
  import akka.actor.Cancellable
  case class Pending(
    key: String,
    value: Option[String],
    sender: ActorRef,
    attempts: Int,
    retry: Cancellable,
    pendingReplicators: Set[ActorRef],
    timeout: Cancellable)
  case class PersistAttempt(key: String, value: Option[String], id: Long)
  val system = akka.actor.ActorSystem("system")

  /*
   * The contents of this actor is just a suggestion, you can implement it in any way you like.
   */

  var kv = Map.empty[String, String]
  // a map from secondary replicas to replicators
  var secondaries = Map.empty[ActorRef, ActorRef]
  // the current set of replicators
  var replicators = Set.empty[ActorRef]

  var updates = Map.empty[Long, Pending]

  var snapReqMap = Map.empty[Long, ActorRef]

  var previousSequences = List.empty[Long]

  val persistence: ActorRef = context.actorOf(persistenceProps)

  arbiter ! Join

  var isLeader: Boolean = false

  def receive = {
    case JoinedPrimary => {
      isLeader = true
      context.become(leader)
    }
    case JoinedSecondary => {
      isLeader = false
      context.become(replica)
    }
  }

  /* TODO Behavior for  the leader role. */
  val leader: Receive = {
    case Insert(key, value, id) => {
      persistAttempt(key, Option(value), id)
    }
    case PersistAttempt(key, value, id) => {
      persistAttempt(key, value, id)
    }
    case Persisted(key, id) => {
      updateKv(key, id)
    }
    case Replicated(key, id) => {
      replicatedOnSecondary(key, id)
    }
    case Remove(key, id) => {
      persistAttempt(key, None, id)
    }
    case OperationFailed(id) => {
      notifyAboutFailedOp(id)
    }
    case Replicas(r) => {
      manageSecondaries(r - self)
    }
    case Get(key, id) => {
      get(key, id)
    }
  }

  /* TODO Behavior for the replica role. */
  val replica: Receive = {
    case Snapshot(key, value, seq) => {
      checkSnapshotSeq(key, value, seq)
    }
    case PersistAttempt(key, value, id) => {
      persistAttempt(key, value, id)
    }
    case Persisted(key, id) => {
      updateKv(key, id)
    }
    case Get(key, id) => {
      get(key, id)
    }
  }

  private def manageSecondaries(secs: Set[ActorRef]): Unit = {
    val deletedSecs = secondaries.keySet.diff(secs)
    val newSecs = secs.diff(secondaries.keySet)

    deletedSecs foreach { deleted =>
      val r = secondaries(deleted)
      r ! PoisonPill
      secondaries -= deleted
      replicators -= r

      updates foreach {
        case (id, p) => {
          if (p.pendingReplicators.nonEmpty) {
            updates += id -> p.copy(pendingReplicators = p.pendingReplicators - r)
          }
        }
      }
    }

    newSecs foreach { s =>
      val replicator = context.actorOf(Replicator.props(s))
      secondaries += ((s, replicator))
      replicators += replicator

      for {
        (r, index) <- replicators.zipWithIndex
        (k, v) <- kv
      } yield r ! Replicate(k, Option(v), index)
    }
  }

  private def notifyAboutFailedOp(id: Long): Unit = {
    val failed = updates(id)

    failed.retry.cancel
    failed.sender ! OperationFailed(id)
  }

  private def replicatedOnSecondary(key: String, id: Long): Unit = {
    val p = updates(id)
    updates += id -> p.copy(pendingReplicators = p.pendingReplicators - sender)
    self ! Persisted(key, id)
  }

  private def updateKv(key: String, id: Long): Unit = {
    if (updates.contains(id)) {
      val persisted = updates(id)
      if (persisted.pendingReplicators.isEmpty) {
        if (isLeader) updates -= id
        persisted.retry.cancel
        persisted.timeout.cancel

        if (!isLeader) {
          snapReqMap(id) ! SnapshotAck(key, id)
        } else {
          persisted.sender ! OperationAck(id)
        }
      }
    }
  }

  private def get(key: String, id: Long): Unit = {
    sender ! GetResult(key, kv.get(key), id)
  }

  private def checkSnapshotSeq(key: String, value: Option[String], id: Long): Unit = {
    if (previousSequences.isEmpty && id != 0) {
      // we don't want to send anything
    } else if ((previousSequences.isEmpty && id == 0) ||
      (!previousSequences.contains(id) &&
        previousSequences.forall(x => id > x) &&
        id == previousSequences.head + 1)) {
      self ! PersistAttempt(key, value, id)
      snapReqMap += ((id, sender))
      previousSequences = id :: previousSequences
    } else {
      // if we re-use the same id number, we immediately send an ack
      sender ! SnapshotAck(key, id)
    }
  }

  private def persistAttempt(key: String, value: Option[String], id: Long): Unit = {
    if (updates.contains(id) && updates(id).attempts >= 10) {
      self ! OperationFailed(id)
    } else {
      persistence ! Persist(key, value, id)

      val retryTimeout = system.scheduler.scheduleOnce(100 milliseconds) {
        self ! PersistAttempt(key, value, id)
      }

      updates.get(id) match {
        case Some(p) => {
          updates += id -> p.copy(attempts = p.attempts + 1).copy(retry = retryTimeout)
        }
        case None => {
          val timeout = system.scheduler.scheduleOnce(1 second) { self ! OperationFailed(id) }
          updates += id -> Pending(key, value, sender, 0, retryTimeout, replicators, timeout)
          replicators foreach { _ ! Replicate(key, value, id) }
          value match {
            case Some(value) => kv += ((key, value))
            case None => kv -= key
          }
        }
      }
    }
  }
}
