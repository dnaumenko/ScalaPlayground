package cats
package effect

import cats.data.{IndexedStateT, StateT}
import cats.mtl.MonadState
import com.example.functional.cats.UpdateLocalContextProblem_StateT.{Ctx, StateIO}

import scala.language.higherKinds
import scala.util.Either

object StateTConcurrent {
  implicit def catsStateTConcurrent(implicit C: Concurrent[IO], A: Async[StateIO], state: MonadState[StateIO, Ctx]): Concurrent[StateIO] = {
    new Concurrent[StateIO] {
      // Ignores child context
      override def start[A](fa: StateIO[A]): StateIO[effect.Fiber[StateIO, A]] = StateT(startCtx => {
        C.start(fa.run(startCtx)).map {
          fiber => startCtx -> fiberT(fiber)
        }
      })

      def fiberT[A](fiber: Fiber[IO, (Ctx, A)]): Fiber[StateIO, A] = {
        val join: StateT[IO, Ctx, A] = StateT((_: Ctx) => fiber.join)
        val cancel = StateT[IO, Ctx, Unit](ctx => fiber.cancel.map(ctx -> _))
        Fiber(join, cancel)
      }

      // Race pairs but ignores a context written by them, so only parent context gets returned
      override def racePair[A, B](fa:  StateIO[A], fb:  StateIO[B]): StateIO[Either[(A, effect.Fiber[StateIO, B]), (effect.Fiber[StateIO, A], B)]] = {
        StateT { startCtx =>
          C.racePair(fa.run(startCtx), fb.run(startCtx)).map {
            case Left(((_, value), fiber)) =>
              (startCtx, Left((value, fiberT(fiber))))
            case Right((fiber, (_, value))) =>
              (startCtx, Right((fiberT(fiber), value)))
          }
        }
      }

      override def async[A](k:  (Either[Throwable, A] => Unit) => Unit): StateIO[A] = A.async(k)
      override def asyncF[A](k:  (Either[Throwable, A] => Unit) => StateIO[Unit]): StateIO[A] = A.asyncF(k)
      override def suspend[A](thunk:  => StateIO[A]): StateIO[A] = A.suspend(thunk)
      override def bracketCase[A, B](acquire:  StateIO[A])(use:  A => StateIO[B])(release:  (A, ExitCase[Throwable]) => StateIO[Unit]): StateIO[B] = {
        A.bracketCase(acquire)(use)(release)
      }
      override def raiseError[A](e:  Throwable): StateIO[A] = A.raiseError(e)
      override def handleErrorWith[A](fa:  StateIO[A])(f:  Throwable => StateIO[A]): StateIO[A] = A.handleErrorWith(fa)(f)
      override def pure[A](x:  A): StateIO[A] = A.pure(x)
      override def flatMap[A, B](fa:  StateIO[A])(f:  A => StateIO[B]): StateIO[B] = A.flatMap(fa)(f)
      override def tailRecM[A, B](a:  A)(f:  A => StateIO[Either[A, B]]): StateIO[B] = A.tailRecM(a)(f)
    }
  }
}
