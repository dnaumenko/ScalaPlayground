package cats.effect

import java.util.concurrent.Executors

import cats.Monad
import cats.implicits._

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

object TasksInf {
  def taskHeavy(prefix: String): IO[Nothing] = {
    Monad[IO].tailRecM(0) { i => for {
      _ <- IO.delay(println(s"${Thread.currentThread().getName}; $prefix: $i"))
      _ <- IO.delay(Thread.sleep(1000))
    } yield Left(i + 1)}
  }

  def taskLight(prefix: String)(implicit T: Timer[IO]): IO[Nothing] = {
    Monad[IO].tailRecM(0) { i => for {
      _ <- IO.delay(println(s"${Thread.currentThread().getName}; $prefix: $i"))
      _ <- IO.sleep(1.second) // <- note async style sleep
    } yield Left(i + 1)}
  }

  def bunch(n: Int)(gen: String => IO[Nothing])(implicit CS: ContextShift[IO]): IO[List[Fiber[IO, Nothing]]] = {
    (1 to n).toList.map(i => s"Task $i").traverse(gen(_).start)
  }
}

object TasksHeavy {
  def taskHeavy(name: String): Int = {
    Thread.sleep(1000)
    println(s"${Thread.currentThread.getName}: $name: Computed!")
    42
  }

  def sync(name: String): IO[Int] = IO.delay(taskHeavy(name))

  def async(name: String)(implicit C: ContextShift[IO]): IO[Int] =
    C.shift *> IO.async { cb => // shift to blocking pool! Alternative to new Thread(...).start()
      cb(Right(taskHeavy(name)))
    }

  def benchmark[A](io: IO[A])(implicit T: Timer[IO]): IO[(A, Long)] = for {
    tStart <- T.clock.monotonic(SECONDS)
    res <- io
    tEnd <- T.clock.monotonic(SECONDS)
  } yield (res, tEnd - tStart)

  def benchmarkFlush[A](io: IO[A])(implicit T: Timer[IO]): IO[Unit] = {
    benchmark(io).map {
      case (res, time) => println(s"Computed result $res in $time seconds")
    }
  }

  def bunch(n: Int)(gen: String => IO[Int])(implicit CS: ContextShift[IO]): IO[List[Int]] = {
    (1 to n).toList.map(i => s"Task $i")
      .traverse(gen(_).start)
      .flatMap(_.traverse(_.join))
  }
}

object WorkingWithFibers {
  def sum(from: Int, to: Int)(implicit T: Timer[IO]): IO[Int] = Monad[IO].tailRecM((from, 0)) {
    case (i, runningTotal) =>
      if (i == to) IO.pure(Right(runningTotal + 1))
      else if (i > to) IO.pure(Right(runningTotal))
      else for {
        _ <- IO(println(s"${Thread.currentThread().getName} Running total from $from to $to, currently at $i: $runningTotal"))
        _ <- IO.sleep(500.millis)
      } yield Left((i + 1, runningTotal + i))
  }

  def sequential(implicit T: Timer[IO]): IO[Int] = for {
    s1 <- sum(1, 10)
    s2 <- sum(10, 20)
  } yield s1 + s2

  def sequentialTraverse(implicit T: Timer[IO]): IO[Int] = {
    List(sum(1, 10), sum(10, 20)).sequence.map(_.sum)
  }

  def parallel(implicit CS: ContextShift[IO], T: Timer[IO]): IO[Int] = for {
    f1 <- sum(1, 10).start
    f2 <- sum(10, 20).start
    s1 <- f1.join
    s2 <- f2.join
  } yield s1 + s2
}

object Concurrency extends App {
  val bec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())
  val bcs: ContextShift[IO] = IO.contextShift(bec)

  implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newFixedThreadPool(2))
  implicit val cs: ContextShift[IO] = IO.contextShift(ec)
  implicit val timer: Timer[IO] = IO.timer(ec)

  import TasksHeavy._
  import WorkingWithFibers._

  benchmarkFlush(parallel).unsafeRunSync()
}
