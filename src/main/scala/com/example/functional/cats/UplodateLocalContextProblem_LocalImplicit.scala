package com.example.functional.cats

import cats.data.ReaderT
import cats.effect.{ContextShift, IO, Sync}
import fs2.Stream

object ReaderIOPackage {
  type Ctx = String

  type ReaderIO[A] = ReaderT[IO, Ctx, A]
  def ReaderIO[A](run: Ctx => IO[A]): ReaderIO[A] = ReaderT.apply(run)

  object HasCtx {
    def apply[A, F[_]](implicit c: HasCtx[A, F]): HasCtx[A, F] = c
  }

  trait HasCtx[A, F[_]] {
    def ctx: F[Ctx]
  }

  implicit def hasCtx[A]: HasCtx[A, ReaderIO] = new HasCtx[A, ReaderIO] {
    override def ctx: ReaderIO[Ctx] = ReaderIO(s => IO.pure(s + "!"))
  }
}

object PackagedConsumer {
  import ReaderIOPackage._
  type Header = String

  case class Message(payload: String, header: Header)

  case class Consumer[F[_] : Sync](messages: Seq[Message]) {
    def consume(): Stream[F, Message] = {
      Stream.emits[F, Message](messages)
    }
  }

  implicit def hasMsgCtx(implicit msg: Message): HasCtx[Message, ReaderIO] = new HasCtx[Message, ReaderIO] {
    println("Create Implicit")

    def toCtx(header: Header): Ctx = s"$header-updated"
    override def ctx: ReaderIO[Ctx] = ReaderIO(s => IO.pure(toCtx(msg.header)))
  }
}

object UpdateLocalContextProblem_LocalImplicitOverride extends App {
  import ReaderIOPackage._
  import PackagedConsumer._

  implicit val ioCs: ContextShift[IO] = IO.contextShift(scala.concurrent.ExecutionContext.global)

//  val result = ReaderIO(_ => IO.pure(Message("value1", "header1"))).flatMap {
//    implicit msg => HasCtx[Message, ReaderIO].ctx
//  }.flatMapF {
//    c => IO.delay(println(c))
//  }
//
//  result.run("init").unsafeRunSync()

  val result2 = Consumer[ReaderIO](Seq(Message("value1", "header1")))
    .consume()
    .evalMap { implicit msg: Message =>
      HasCtx[Message, ReaderIO].ctx
    }
    .compile.toList.run("ctx").unsafeRunSync()

  println(result2)
}
