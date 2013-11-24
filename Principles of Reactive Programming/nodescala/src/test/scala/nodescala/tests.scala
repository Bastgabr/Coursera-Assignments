package nodescala

import scala.language.postfixOps
import scala.util.{ Try, Success, Failure }
import scala.collection._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.async.Async.{ async, await }
import org.scalatest._
import NodeScala._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class NodeScalaSuite extends FunSuite {

  test("A Future should always be created") {
    val always = Future.always(517)

    assert(Await.result(always, 0 nanos) == 517)
  }

  test("A Future should never be created") {
    val never = Future.never[Int]

    try {
      Await.result(never, 1 second)
      assert(false)
    } catch {
      case t: TimeoutException => // ok!
    }
  }

  test("All Futures should be true") {
    val f1 = Future.always(1)
    val f2 = Future.always(2)
    val f3 = Future.always(3)
    val fs = List(f1, f2, f3)
    val all = Future.all(fs)
    val expected = List(1, 2, 3)
    assert(Await.result(all, 1 second) === expected)
  }

  test("Trying to take value of Future.never should throw TimeoutException") {
    intercept[TimeoutException] {
      val fs = List(Future.always { 1 }, Future.always { 2 }, Future.never)
      val fa = Future.all(fs)
      Await.result(fa, 1 second)
    }
  }

  test("Any should return the first completed future") {
    val durs = List.fill(5)(math.random).zipWithIndex
    // E.g., List[(Double, Int)] = List((0.29697330541798195,0), (0.6334949214823781,1), (0.13345745202036496,2), (0.6935523459156602,3), (0.012216707302845187,4))

    val minIndex = durs.minBy { _._1 }._2
    // E.g., 4

    val fxs = durs.map {
      case (dur, idx) => Future {
        blocking {
          Thread.sleep((dur * 1000L).toLong)
        }
        idx
      }
    }

    val any = Future.any(fxs)

    assert(Await.result(any, 1 second) == minIndex)
  }

  test("Any should return the first completed future, or error") {
    val any = Future.any(List(Future.never[Int], Future.always(1)))
    try {
      assert(Await.result(any, 5 seconds) == 1)
    } catch {
      case t: Throwable => fail
    }

    val any2 = Future.any(List(
      Future {
        blocking {
          Thread.sleep(500)
        }; 1
      },
      Future {
        blocking {
          Thread.sleep(200)
        }; 2
      }))

    try {
      assert(Await.result(any2, 5 seconds) == 2)
    } catch {
      case t: Throwable => fail
    }

    val any3 = Future.any(List(
      Future {
        blocking {
          Thread.sleep(500)
        }; 1
      },
      Future {
        blocking {
          Thread.sleep(200)
        }; 2
      },
      Future {
        throw new Exception
      }))

    try {
      Await.result(any3, 5 seconds)
      fail
    } catch {
      case t: Throwable => //ok
    }
  }

  test("A Future should not complete before given delay") {
    val f1 = Future.delay(3 seconds)

    try {
      Await.result(f1, 1 seconds)
      fail
    } catch {
      case t: Throwable => //ok
    }
  }

  test("Test now") {
    assert(1 == Future.always(1).now)
    try {
      Future.never.now
      assert(false)
    } catch {
      case t: NoSuchElementException => // ok!
    }
    try {
      Future.delay(10 seconds).now
      assert(false, "Should throw NoSuchElementException")
    } catch {
      case t: NoSuchElementException => // ok!
    }
  }

  test("Continue should handle exception") {
    val f = future(throw new IllegalStateException())
    val s = f.continue(_ => "Hello")
    assert("Hello" === Await.result(s, 100 nanosecond))
  }

  test("A Future should be continued") {
    val result = Future[String] {
      throw new IllegalStateException()
    }.continueWith { f =>
      "continued"
    }
    assert(Await.result(result, 1 second) == "continued")
  }

  test("CancellationTokenSource should allow stopping the computation") {
    val cts = CancellationTokenSource()
    val ct = cts.cancellationToken
    val p = Promise[String]()

    async {
      while (ct.nonCancelled) {
        // do work
      }

      p.success("done")
    }

    cts.unsubscribe()
    assert(Await.result(p.future, 1 second) == "done")
  }

  test("Test Future.run with Promise") {

    val p = Promise[Boolean]()

    val working = Future.run() {
      ct =>
        async {
          while (ct.nonCancelled) {
            println("working")
            Thread.sleep(200)
          }
          println("done")
          p.success(true)
        }
    }

    Future.delay(1 seconds) onComplete {
      case _ => working.unsubscribe()
    }

    assert(Await.result(p.future, 2 second))
  }

  class DummyExchange(val request: Request) extends Exchange {
    @volatile var response = ""
    val loaded = Promise[String]()
    def write(s: String) {
      response += s
    }
    def close() {
      loaded.success(response)
    }
  }

  class DummyListener(val port: Int, val relativePath: String) extends NodeScala.Listener {
    self =>

    @volatile private var started = false
    var handler: Exchange => Unit = null

    def createContext(h: Exchange => Unit) = this.synchronized {
      assert(started, "is server started?")
      handler = h
    }

    def removeContext() = this.synchronized {
      assert(started, "is server started?")
      handler = null
    }

    def start() = self.synchronized {
      started = true
      new Subscription {
        def unsubscribe() = self.synchronized {
          started = false
        }
      }
    }

    def emit(req: Request) = {
      val exchange = new DummyExchange(req)
      if (handler != null) handler(exchange)
      exchange
    }
  }

  class DummyServer(val port: Int) extends NodeScala {
    self =>
    val listeners = mutable.Map[String, DummyListener]()

    def createListener(relativePath: String) = {
      val l = new DummyListener(port, relativePath)
      listeners(relativePath) = l
      l
    }

    def emit(relativePath: String, req: Request) = this.synchronized {
      val l = listeners(relativePath)
      l.emit(req)
    }
  }

  test("Listener should serve the next request as a future") {
    val dummy = new DummyListener(8191, "/test")
    val subscription = dummy.start()

    def test(req: Request) {
      val f = dummy.nextRequest()
      dummy.emit(req)
      val (reqReturned, xchg) = Await.result(f, 1 second)

      assert(reqReturned == req)
    }

    test(immutable.Map("StrangeHeader" -> List("StrangeValue1")))
    test(immutable.Map("StrangeHeader" -> List("StrangeValue2")))

    subscription.unsubscribe()
  }

  test("Server should serve requests") {
    val dummy = new DummyServer(8191)
    val dummySubscription = dummy.start("/testDir") {
      request => for (kv <- request.iterator) yield (kv + "\n").toString
    }

    // wait until server is really installed
    Thread.sleep(500)

    def test(req: Request) {
      val webpage = dummy.emit("/testDir", req)
      val content = Await.result(webpage.loaded.future, 1 second)
      val expected = (for (kv <- req.iterator) yield (kv + "\n").toString).mkString
      assert(content == expected, s"'$content' vs. '$expected'")
    }

    test(immutable.Map("StrangeRequest" -> List("Does it work?")))
    test(immutable.Map("StrangeRequest" -> List("It works!")))
    test(immutable.Map("WorksForThree" -> List("Always works. Trust me.")))

    dummySubscription.unsubscribe()
  }

}




