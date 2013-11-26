package nodescala

import scala.language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import ExecutionContext.Implicits.global
import scala.async.Async.{ async, await }

object Main {

  def main(args: Array[String]) {
    // TO IMPLEMENT
    // 1. instantiate the server at 8191, relative path "/test",
    //    and have the response return headers of the request
    val myServer = new NodeScala.Default(8191)

    val myServerSubscription = myServer.createListener("/test").start

    // TO IMPLEMENT
    // 2. create a future that expects some user input `x`
    //    and continues with a `"You entered... " + x` message
    val userInterrupted: Future[String] = {
      Future.userInput("").map(x => "You entered... " + x)
    }

    // TO IMPLEMENT
    // 3. create a future that completes after 20 seconds
    //    and continues with a `"Server timeout!"` message
    val timeOut: Future[String] = Future.delay(20 seconds).continueWith(_ => "Server timeout!")

    // TO IMPLEMENT
    // 4. create a future that completes when either 10 seconds elapse
    //    or the user enters some text and presses ENTER
    val terminationRequested: Future[String] = {
      val tenSecondsElapsedFuture = Future.delay(10 seconds).continueWith(_ => "")
      val userInputsFuture = Future.userInput("")
      Future.any(List(tenSecondsElapsedFuture, userInputsFuture))
    }

    // TO IMPLEMENT
    // 5. unsubscribe from the server
    terminationRequested onSuccess {
      case msg => {
        myServerSubscription.unsubscribe
        println("Bye")
      }
    }

    try {
      Await.result(terminationRequested, 1 minute)
    } catch {
      case t: Throwable => //ok
    }
  }

}