package cats.effect

//import java.io.{BufferedReader, FileReader}
//import java.util.concurrent.{ExecutorService, Executors}
//
//import cats.data.ReaderT
//
//import scala.concurrent.ExecutionContext
//import cats.implicits._

object ResourceExperiments extends App {

//  val F = Sync[IO]
//  Resource.fromAutoCloseable(F.delay {
//    new BufferedReader(new FileReader("example.file"))
//  }).allocated
//
//  type Context = String
//  type ReaderIO[A] = ReaderT[IO, Context, A]
//
//  def allocateThreadPool[F[_]](size: Int)(implicit S: Sync[F]): Resource[F, ExecutionContext] = {
//    val alloc = S.delay(Executors.newFixedThreadPool(size))
//    val free  = (es: ExecutorService) => S.delay(es.shutdown())
//    Resource.make(alloc)(free).map(ExecutionContext.fromExecutor)
//  }
//
//  case class Services()
//  val services: Resource[ReaderIO, Services] = for {
//    _ <- allocateThreadPool[ReaderIO](size = 10)
//    _ <- allocateThreadPool[ReaderIO](size = 20)
//  } yield Services()
//
//  val test: ReaderIO[(Services, ReaderIO[Unit])] = services.allocated
//
//  val testIO: IO[(Services, IO[Unit])] = test.run(null).map {
//    case (s, rio) => s -> rio.run(null)
//  }
//
//  testIO
}
