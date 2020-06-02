import cats.effect._
import cats.effect.concurrent.MVar
import scala.concurrent.duration._
import cats.syntax.all._

object CatsEffect extends IOApp{
  def runCounter(mvar: MVar[IO, Int]): Resource[IO, Unit] = {
    def rec(counter: Int): IO[Unit] = for {
      _ <- mvar.put(counter)
      _ <- IO.sleep(1.second)
      _ <- rec(counter + 1)
    } yield ()

    Resource.make(rec(0).start)(_.cancel).void
  }

  def runPrinter(mvar: MVar[IO, Int]): Resource[IO, Unit] = {
    def rec: IO[Unit] = for {
      value <- mvar.take
      _ <- IO(println(value))
      _ <- rec
    } yield ()

    Resource.make(rec.start)(_.cancel).void
  }

  val gSP: Resource[IO, Unit] = for {
    mvar <- Resource.make(MVar.empty[IO, Int])(_ => IO.unit)
    _ <- runPrinter(mvar)
    _ <- runCounter(mvar)
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    gSP.use(_ => IO.never)
}
