package com.example

import scalaz.zio.App
import scalaz.zio.console._

object Playground extends App {
  def run(args: List[String]) =
    myAppLogic.fold(_ => 1, _ => 0)

  val myAppLogic =
    for {
      _ <- putStrLn("Hello! What is your name?")
      n <- getStrLn
      _ <- putStrLn(s"Hello, ${n}, welcome to ZIO!")
    } yield ()
}
