package com.example

import cats.{Applicative, Monad}
import cats.effect.{IO, Sync}
import com.example
import scalaz.zio.console._

//object Playground extends App {
//  def run(args: List[String]) =
//    myAppLogic.fold(_ => 1, _ => 0)
//
//  val myAppLogic =
//    for {
//      _ <- putStrLn("Hello! What is your name?")
//      n <- getStrLn
//      _ <- putStrLn(s"Hello, ${n}, welcome to ZIO!")
//    } yield ()
//}

object Test extends App {
  import cats.syntax._
  import cats.implicits._

  case class Test[F[_] : Sync : Monad]() {
    def doSomething(): F[Unit] = Sync[F].delay {
      println("hi")
    }

    def runSmth(): F[Unit] = {
      for {
        _ <- doSomething()
        a = true
        _ <- {
          if (a)
            doSomething()
          else
            Sync[F].unit
        }
      } yield ()
    }
  }

  Test[IO]().runSmth().unsafeRunSync()

}
