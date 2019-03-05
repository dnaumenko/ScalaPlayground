package com.example

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.util.Try

object Playground extends App {
  import scala.concurrent.ExecutionContext.Implicits.global


  val fut = Future(
    while (true) {
      println("true")
    }
  )

  trait Or[A, B]

  trait A[_]

  class Aa extends A[Int Or Int]

  Try(Await.result(fut, 5.seconds))
}
