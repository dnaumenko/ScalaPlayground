package com.example.functional.cats

import cats.data.{ReaderT, StateT}
import cats.{Applicative, Monad, effect}
import cats.effect.concurrent.Ref
import cats.effect._
import cats.mtl.{ApplicativeLocal, MonadState}
import com.example.functional.cats.UpdateLocalContextProblem_Ref.{Consumer, Ctx, Message, ReaderIO}
import com.example.functional.cats.UpdateLocalContextProblem_StateT.{Ctx, StateIO}
import fs2.Stream

object UpdateLocalContextProblem_Local extends App {
  type Ctx = String
  type Header = String

  type StateIO[A] = ReaderT[IO, Ctx, A]
  case class Message(payload: String, header: Header)

  def toCtx(header: Header): Ctx = s"$header-updated"

  case class Consumer[F[_] : Sync](messages: Seq[Message])(implicit val local: ApplicativeLocal[F, Ctx]) {
    def consume(): Stream[F, Ctx] = {
      Stream.emits(messages).flatMap[F, Message] {
        msg =>
          val ctx = toCtx(msg.header)
          val fun: F[Message] => F[Message] = local.scope[Message](ctx)
          Stream.eval(fun(Sync[F].pure(msg)))
      }.evalMap {
        _ => local.ask // returns ctx, not ctx-updated
      }
    }
  }

  implicit val ioCs = IO.contextShift(scala.concurrent.ExecutionContext.global)
  import cats.mtl.implicits._

  val result = Consumer[StateIO](Seq(Message("value1", "header1"))).consume().compile.toList.run("ctx").unsafeRunSync()
  println(result) // prints List(ctx), but I want to print it ctx-updated
}

object UpdateLocalContextProblem_Ref extends App {
  import cats.implicits._

  type Ctx = String
  type Header = String

  type ReaderIO[A] = ReaderT[IO, Ref[IO, Ctx], A]
  case class Message(payload: String, header: Header)

  def toCtx(header: Header): Ctx = s"$header-updated"

  case class Consumer[F[_] : Sync : LiftIO](messages: Seq[Message])(implicit val local: ApplicativeLocal[F, Ref[IO, Ctx]]) {
    def consume(): Stream[F, Message] = {
      Stream.emits(messages).evalMap[F, Message] {
        msg =>
          val ctx = toCtx(msg.header)

          for {
            ref <- local.ask
            _ <- LiftIO[F].liftIO(ref.set(ctx))
            ref <- local.ask
            refValNew <- LiftIO[F].liftIO(ref.get)
            _ <- Sync[F].delay(println(s"After Set ${Thread.currentThread().getId} -> $refValNew, $msg"))
          } yield msg
      }
    }
  }

  implicit val ioCs = IO.contextShift(scala.concurrent.ExecutionContext.global)
  import cats.mtl.implicits._

  val io = for {
    ref <- Ref.of[IO, Ctx]("ctx")
    result <- Consumer[ReaderIO](1.to(50).map(i => Message(s"value $i", s"header$i")))
      .consume()
      .parEvalMap(10)(msg => for {
        ref <- ApplicativeLocal[ReaderIO, Ref[IO, Ctx]].ask
        ctx <- LiftIO[ReaderIO].liftIO(ref.get)
        _ <- Sync[ReaderIO].delay {
          Thread.sleep(100)
          println(s"Now Thread ${Thread.currentThread().getId} -> $ctx, $msg")
        }
      } yield ())
      .compile.toList.run(ref)
  } yield result

  // There is no mapping between ReaderT's ctx and message. It's completely lost and random!
  println(io.unsafeRunSync()) // prints List(ctx), but I want to print it ctx-updated
}

object UpdateLocalContextProblem_DefferedRefStream extends App {
  type Ctx = String
  type Header = String

  type StateIO[A] = ReaderT[IO, Ctx, A]
  case class Message(payload: String, header: Header)

  def toCtx(header: Header): Ctx = s"$header-updated"

  case class Consumer[F[_] : Sync](messages: Seq[Message])(implicit val local: ApplicativeLocal[F, Ctx]) {
    def consume(): Stream[F, Ctx] = {
      Stream.emits(messages).flatMap[F, Message] {
        msg =>
          val ctx = toCtx(msg.header)
          val fun: F[Message] => F[Message] = local.scope[Message](ctx)
          Stream.eval(fun(Sync[F].pure(msg)))
      }.evalMap {
        _ => local.ask // returns ctx, not ctx-updated
      }
    }
  }

  implicit val ioCs = IO.contextShift(scala.concurrent.ExecutionContext.global)
  import cats.mtl.implicits._

  val result = Consumer[StateIO](Seq(Message("value1", "header1"))).consume().compile.toList.run("ctx").unsafeRunSync()
  println(result) // prints List(ctx), but I want to print it ctx-updated
}

// No Concurrent Impl
object UpdateLocalContextProblem_StateT extends App {
  import cats.mtl.implicits._
  import cats.syntax._

  type Ctx = String
  type Header = String

  type StateIO[A] = StateT[IO, Ctx, A]
  case class Message(payload: String, header: Header)

  def toCtx(header: Header): Ctx = s"$header-updated"

  case class Consumer[F[_] : Sync : Monad](messages: Seq[Message])(implicit val local: MonadState[F, Ctx]) {
    def consume(): Stream[F, Message] = {
      Stream.emits[F, Message](messages).evalMap {
        msg =>
          val ctx = toCtx(msg.header)
          Sync[F].map(local.set(ctx)) {
            _ => msg
          }
      }
    }
  }

  implicit val ioCs = IO.contextShift(scala.concurrent.ExecutionContext.global)
  import cats.effect.StateTConcurrent._

  val io = for {
    _ <- StateT[IO, Ctx, Message](ctx => IO.pure(ctx -> Message(s"value 1", s"header 1")))
    _ <- MonadState[StateIO, Ctx].inspect(s => println(s"Init: $s"))
    _ <- MonadState[StateIO, Ctx].set("Updated Context")
    _ <- MonadState[StateIO, Ctx].inspect(s => println(s"UpdateFirst: $s"))
    _ <- Concurrent[StateIO].racePair(
        for {
          _ <- MonadState[StateIO, Ctx].set("Updated in Fiber Context 1")
          _ <- MonadState[StateIO, Ctx].inspect(s => println(s"Inspect in Fiber After Update in Fiber: $s"))
        } yield ()
      , MonadState[StateIO, Ctx].set("Updated in Fiber Context 2")
    )
    _ <- Concurrent[StateIO].racePair(MonadState[StateIO, Ctx].inspect(s => println(s"Inspect in Fiber: $s")), MonadState[StateIO, Ctx].inspect(s => println(s"Inspect in Fiber: $s")))
    _ <- StateT.liftF(IO.delay(Thread.sleep(1000)))
    _ <- MonadState[StateIO, Ctx].inspect(s => println(s"Inspect in parent: $s"))
  } yield ()
  io.run("initial").unsafeRunSync()

  println("---------------")

  val ioStream: Stream[StateIO, Message] = Stream
    .emits[StateIO, Message](1.to(1).map(i => Message(s"value $i", s"header$i")))
    .evalTap(_ => MonadState[StateIO, Ctx].inspect(s => println(s"Init: $s")))
    .evalTap(_ => MonadState[StateIO, Ctx].set("Updated Context"))
    .evalTap(_ => MonadState[StateIO, Ctx].inspect(s => println(s"UpdateFirst: $s")))
    .evalMap(msg => for {
      _ <- MonadState[StateIO, Ctx].inspect(s => println(s"Inspect in Fiber: $s"))
    } yield msg)
  ioStream.compile.drain.run("init").unsafeRunSync()
}
