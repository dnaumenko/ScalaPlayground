package com.example.functional.cats

import cats.data.{ReaderT, StateT}
import cats.effect.concurrent.Ref
import cats.effect._
import cats.mtl.{ApplicativeLocal, MonadState}
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
            refVal <- LiftIO[F].liftIO(ref.get)
//            _ <- Sync[F].delay(println(s"Was Thread ${Thread.currentThread().getId} -> $refVal, $msg"))
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

  case class Consumer[F[_] : Sync](messages: Seq[Message])(implicit val local: MonadState[F, Ctx]) {
    def consume(): Stream[F, Ctx] = {
      Stream.emits[F, Message](messages).evalMap {
        msg =>
          val ctx = toCtx(msg.header)
          val a: F[Message] = Sync[F].map(local.set(ctx)) {
            _ => msg
          }

          a
      }.evalMap {
        _ => local.get
      }
    }
  }

  implicit val ioCs = IO.contextShift(scala.concurrent.ExecutionContext.global)

  implicit def catsStateTConcurrent[F[_]: Concurrent, S, A]: Concurrent[StateIO] = {
    val C = Concurrent[IO]

    new Concurrent[StateIO] {
      override def start[A](fa: StateIO[A]): StateIO[Fiber[StateIO, A]] = {
        StateT[IO, Ctx, A](s => C.start(fa.run(s)).map {
          fiber => (s, fi)
        })
      }
      override def racePair[A, B](fa: StateIO[A], fb: StateIO[B]): StateIO[Either[(A, Fiber[StateIO, B]), (Fiber[StateIO, A], B)]] = ???
      override def async[A](k: (Either[Throwable, A] => Unit) => Unit): StateIO[A] = ???
      override def asyncF[A](k: (Either[Throwable, A] => Unit) => StateIO[Unit]): StateIO[A] = ???
      override def suspend[A](thunk: =>StateIO[A]): StateIO[A] = ???
      override def bracketCase[A, B](acquire: StateIO[A])(use: A => StateIO[B])(release: (A, ExitCase[Throwable]) => StateIO[Unit]): StateIO[B] = ???
      override def raiseError[A](e: Throwable): StateIO[A] = ???
      override def handleErrorWith[A](fa: StateIO[A])(f: Throwable => StateIO[A]): StateIO[A] = ???
      override def pure[A](x: A): StateIO[A] = ???
      override def flatMap[A, B](fa: StateIO[A])(f: A => StateIO[B]): StateIO[B] = ???
      override def tailRecM[A, B](a: A)(f: A => StateIO[Either[A, B]]): StateIO[B] = ???
    }
  }

  /*
    private[effect] trait StateTConcurrent[F[_], S] extends Async.StateTAsync[F, S]
    with Concurrent[StateT[F, S, ?]] {

    override protected implicit def F: Concurrent[F]
    override protected def FA = F

    // Needed to drive static checks, otherwise the
    // compiler will choke on type inference :-(
    type Fiber[A] = cats.effect.Fiber[StateT[F, S, ?], A]

    override def cancelable[A](k: (Either[Throwable, A] => Unit) => CancelToken[StateT[F, S, ?]]): StateT[F, S, A] =
      StateT[F, S, A](s => F.cancelable[A](cb => k(cb).run(s).map(_ => ())).map(a => (s, a)))(F)

    override def start[A](fa: StateT[F, S, A]): StateT[F, S, Fiber[A]] =
      StateT(s => F.start(fa.run(s)).map { fiber => (s, fiberT(fiber)) })

    override def racePair[A, B](fa: StateT[F, S, A], fb: StateT[F, S, B]): StateT[F, S, Either[(A, Fiber[B]), (Fiber[A], B)]] =
      StateT { startS =>
        F.racePair(fa.run(startS), fb.run(startS)).map {
          case Left(((s, value), fiber)) =>
            (s, Left((value, fiberT(fiber))))
          case Right((fiber, (s, value))) =>
            (s, Right((fiberT(fiber), value)))
        }
      }

    protected def fiberT[A](fiber: effect.Fiber[F, (S, A)]): Fiber[A] =
      Fiber(StateT(_ => fiber.join), StateT.liftF(fiber.cancel))
  }
   */

  implicit val conc = new Concurrent[StateIO]() {
    implicit val CE = implicitly[ConcurrentEffect[IO]]

    override def start[A](fa: StateIO[A]): StateIO[Fiber[StateIO, A]] = CE.start(fa)

    override def racePair[A, B](fa: StateIO[A], fb: StateIO[B]): StateIO[Either[(A, Fiber[StateIO, B]), (Fiber[StateIO, A], B)]] = ???

    override def async[A](k: (Either[Throwable, A] => Unit) => Unit): StateIO[A] = ???

    override def asyncF[A](k: (Either[Throwable, A] => Unit) => StateIO[Unit]): StateIO[A] = ???

    override def suspend[A](thunk: =>StateIO[A]): StateIO[A] = ???

    override def bracketCase[A, B](acquire: StateIO[A])(use: A => StateIO[B])(release: (A, ExitCase[Throwable]) => StateIO[Unit]): StateIO[B] = ???

    override def raiseError[A](e: Throwable): StateIO[A] = ???

    override def handleErrorWith[A](fa: StateIO[A])(f: Throwable => StateIO[A]): StateIO[A] = ???

    override def pure[A](x: A): StateIO[A] = ???

    override def flatMap[A, B](fa: StateIO[A])(f: A => StateIO[B]): StateIO[B] = ???

    override def tailRecM[A, B](a: A)(f: A => StateIO[Either[A, B]]): StateIO[B] = ???
  }

  val result = Consumer[StateIO](Seq(Message("value1", "header1")))
    .consume()
    .parEvalMap(10)(m => Sync[StateIO].delay(println(m)))
    .compile.toList.run("ctx").unsafeRunSync()
  println(result._1) // prints List(ctx), but I want to print it ctx-updated
}
