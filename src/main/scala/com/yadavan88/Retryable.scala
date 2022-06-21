package com.yadavan88

import cats.effect.IOApp
import cats.effect.IO
import scala.util.Random
import java.util.UUID
import scala.concurrent.duration._
import IORetryExtension._
import retry.RetryDetails.GivingUp
import java.io.IOException
import cats.effect.std
import retry.syntax.all._

object Main extends IOApp.Simple {

  def requestAuthToken: IO[String] = {
    val url = "http://localhost:9000/app/authenticate"
    IO.defer {
      // make the http request
      println("making the http request")
      val random = Random.nextInt(500)
      if (random != 0) {
        println("uh oh.. this will fail... " + random)
        Random.nextBoolean() match {
          case b if b => IO.raiseError(new Exception("Serious exception"))
          case _      => IO.raiseError(new IOException("Connection exception"))
        }
      } else {
        println("received response successfully")
        IO(UUID.randomUUID().toString())
      }
    }
  }

  def takeExam: IO[Int] = {
    IO.delay(Random.nextInt(100))
  }

  val flakyRequestSimple: IO[String] =
    requestAuthToken.simpleRetry(5, 500.millis)

  import retry._
  val retryPolicyWithLimit = RetryPolicies.limitRetries[IO](5)
  val retryPolicyWithDelay = RetryPolicies.constantDelay[IO](500.millis)
  val limitWithDelayPolicy = retryPolicyWithLimit.join(retryPolicyWithDelay)
  import cats.implicits._
  val limitWithDelayPolicySymbol = retryPolicyWithLimit |+| retryPolicyWithDelay
  val limitWithDelayPolicyMapped = retryPolicyWithLimit.mapDelay(_ + 2.second)
  val limitWithBackOff =
    RetryPolicies.exponentialBackoff[IO](1.second).join(retryPolicyWithLimit)

  val followUpRetryPolicy = limitWithDelayPolicy.followedBy(RetryPolicies.fibonacciBackoff(1.seconds))  

  def onErrorFn(e: Throwable, details: RetryDetails) = {
    details match {
      case ret: RetryDetails.WillDelayAndRetry =>
        IO.println("error occurred..... " + details)
      case GivingUp(totalRetries, totalDelay) =>
        IO.println("done... i will not retry...")
    }
  }

  def canRetryRequest(e: Throwable) = {
    e match {
      case _: IOException => IO.println("Can retry this one...") >> IO(true)
      case _              => IO.println("No point in retrying") >> IO(false)
    }
  }

  def isPassed(mark: Int) = {
    IO.delay(mark > 60)
  }

  def failureLogger(mark: Int, det: RetryDetails) =
    IO.println("failed this time with mark: " + mark)

  val retryWithErrorHandling =
    retryingOnAllErrors(limitWithDelayPolicyMapped, onErrorFn)(requestAuthToken)

  val retryWithErrorHandlingSugar =
    requestAuthToken.retryingOnAllErrors(limitWithDelayPolicyMapped, onErrorFn)

  val retryWithErrorHandlingBackoff =
    retryingOnAllErrors(limitWithBackOff, onErrorFn)(requestAuthToken)

  val retryWithSomeErrors =
    retryingOnSomeErrors(limitWithDelayPolicy, canRetryRequest, onErrorFn)(
      requestAuthToken
    )

  val retryWithSomeFailures =
    retryingOnFailures(limitWithDelayPolicy, isPassed, failureLogger)(takeExam)
  // retryWithSomeFailures.map(finalMark => println("Final Mark = "+finalMark)).void

  val retryWithFollowedAction = requestAuthToken.retryingOnAllErrors(limitWithDelayPolicySymbol, onErrorFn)

  override def run: IO[Unit] = retryWithFollowedAction.void

  // retryWithErrorHandling.void

}
