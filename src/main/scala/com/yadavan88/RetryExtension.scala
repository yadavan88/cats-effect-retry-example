package com.yadavan88

import scala.concurrent.duration.FiniteDuration
import cats.effect.IO

object IORetryExtension {
  implicit class Retryable[A](io: IO[A]) {
    def simpleRetry(noOfRetries: Int, sleep: FiniteDuration): IO[A] = {
      def retryLoop(times: Int): IO[A] = {
        io.map(identity).handleErrorWith { case ex =>
          if (times != 0) {
            println("Will retry in " + sleep)
            IO.sleep(sleep) >> retryLoop(times - 1)
          } else {
            println(
              "Exhausted all the retry attempts, not trying anymore now...."
            )
            IO.raiseError(ex)
          }
        }
      }
      retryLoop(noOfRetries)
    }
  }
}