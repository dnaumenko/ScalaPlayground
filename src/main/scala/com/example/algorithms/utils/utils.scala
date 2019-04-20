package com.example.algorithms

import scala.language.implicitConversions
import java.util.concurrent.TimeUnit
import scala.concurrent.duration.FiniteDuration

object utils {
  object Timed {
    implicit def toValue[U](timed: Timed[U]): U = timed.value

    def apply[U](f: => U): Timed[U] = {
      val before = System.nanoTime()
      val out = f
      val after = System.nanoTime()
      Timed(out, FiniteDuration.apply(after - before, TimeUnit.NANOSECONDS))
    }
  }

  final case class Timed[U](value: U, duration: FiniteDuration) {
    override def toString: String = s"Value: $value: duration: ${duration.toMillis / 1000.0} seconds"
  }
}
