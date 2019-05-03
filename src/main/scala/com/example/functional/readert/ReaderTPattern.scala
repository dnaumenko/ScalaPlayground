package com.example.functional.readert

import cats.data.ReaderT
import cats.effect.{IO, Sync}
import cats.effect.concurrent.Ref
import cats.implicits._

import scala.language.{higherKinds, implicitConversions}

trait HasSomeString[F[_]] {
  def callScope: F[String]
}

trait UpdateSomeString[F[_]] {
  def update(f: String => String): F[Unit]
}

trait HasCustomCtx[F[_], Ctx] {
  def customCtx: F[Ctx]
}

trait UpdateCustomCtx[F[_], Ctx] {
  def update(f: Ctx => Ctx): F[Unit]
}

case class Env[F[_], Ctx](callScope: Ref[F, String], ctx: Ref[F, Ctx])

object Env {
  implicit def emptyCustomCtx(callScope: Ref[IO, String]): Env[IO, Unit] = new Env[IO, Unit](callScope, Ref.unsafe(()))
}

object CatsReaderIO {
  type ReaderIO[A] = ReaderT[IO, Env[IO, Unit], A]
  def ReaderIO[A](run: Env[IO, Unit] => IO[A]): ReaderIO[A] = ReaderT(run)

  type ReaderIOCtx[Ctx, A] = ReaderT[IO, Env[IO, Ctx], A]
  def ReaderIOCtx[Ctx, A](run: Env[IO, Ctx] => IO[A]): ReaderIOCtx[Ctx, A] = ReaderT(run)

  implicit def hasCallScope[Ctx]: HasSomeString[ReaderIOCtx[Ctx, ?]] = new HasSomeString[ReaderIOCtx[Ctx, ?]] {
    override def callScope: ReaderIOCtx[Ctx, String] = {
      ReaderIOCtx(env => env.callScope.get)
    }
  }

  implicit val updateSomeString: UpdateSomeString[ReaderIO] = new UpdateSomeString[ReaderIO] {
    override def update(f: String => String): ReaderIO[Unit] = {
      ReaderIO(env => env.callScope.update(f))
    }
  }

  implicit def hasCustomCtx[Ctx]: HasCustomCtx[ReaderIOCtx[Ctx, ?], Ctx] = new HasCustomCtx[ReaderIOCtx[Ctx, ?], Ctx] {
    override def customCtx: ReaderIOCtx[Ctx, Ctx] = {
      ReaderIOCtx(env => env.ctx.get)
    }
  }
}

object ReaderTPattern extends App {
  import CatsReaderIO._

  def doSomethingWithCS[F[_] : Sync](implicit CS: HasSomeString[F]) = for {
    cs <- CS.callScope
    _ <- Sync[F].delay(println(s"read cs: $cs"))
  } yield cs

  def doSomethingWithCustomCtx[F[_] : Sync](implicit CS: HasCustomCtx[F, String]) = for {
    cs <- CS.customCtx
    _ <- Sync[F].delay(println(s"read custom ctx: $cs"))
  } yield cs

  val program: ReaderIOCtx[String, Unit] = for {
    _ <- ReaderIOCtx[String, Unit](_ => IO.delay(println("Starting work")))
    _ <- doSomethingWithCS[ReaderIOCtx[String, ?]]
    _ <- doSomethingWithCustomCtx[ReaderIOCtx[String, ?]]
  } yield ()
  program.run(Env(Ref.unsafe("cs_init"), Ref.unsafe("ctx_init"))).unsafeRunSync()

  val program2: ReaderIO[Unit] = for {
    _ <- ReaderIO(_ => IO.delay(println("Starting work")))
    _ <- doSomethingWithCS[ReaderIO]
  } yield ()
  program2.run(Env(Ref.unsafe("cs_init"), Ref.unsafe("ctx_init"))).unsafeRunSync()
  program2.run(Ref.unsafe("cs_init")).unsafeRunSync()
}
