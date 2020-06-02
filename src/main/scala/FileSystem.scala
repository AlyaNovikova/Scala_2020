import java.nio.file.{Files, Path, Paths}

import cats.{Applicative, Id, Monad}
import cats.implicits._

import scala.language.higherKinds
import scala.collection.JavaConverters._

trait MkDir[F[_], Dir] {
  def mkDir(dir: Dir, name: String): F[Dir]
}

trait MkFile[F[_], Dir, File] {
  def mkFile(dir: Dir, name: String): F[File]
}

trait FilesInDir[F[_], Dir, File] {
  def filesInDir(dir: Dir): F[List[File]]
}

trait NameOfFile[F[_], File] {
  def nameOfFile(file: File): F[String]
}

trait MoveFile[F[_], Dir, File] {
  def moveFile(dir: Dir, file: File): F[File]
}

trait Printer[F[_], File] {
  def printName(file: File): F[Unit]
}


class Program [F[_], Dir, File] (implicit
                                 F: Monad[F],
                                 mkDir: MkDir[F, Dir],
                                 mkFile: MkFile[F, Dir, File],
                                 filesInDir: FilesInDir[F, Dir, File],
                                 nameOfFile: NameOfFile[F, File],
                                 moveFile: MoveFile[F, Dir, File],
                                 printer: Printer[F, File]) {
  def run(dir: Dir): F[Unit] = for {
    testDir <- mkDir.mkDir(dir, "test_dir")
    _ <- mkFile.mkFile(testDir, "foo")
    _ <- mkFile.mkFile(testDir, "bar")
    _ <- mkFile.mkFile(testDir, "baz")
    files <- filesInDir.filesInDir(testDir)
    _ <- files.traverse(file => for {
      _ <- printer.printName(file)
      new_name <- nameOfFile.nameOfFile(file)
      new_dir <- mkDir.mkDir(testDir, new_name.head.toString)
      _ <- moveFile.moveFile(new_dir, file)
    } yield())
  } yield()
}

class RealFileSystem[F[_] : Applicative] extends MkDir[F, Path]
  with MkFile[F, Path, Path]
  with FilesInDir[F, Path, Path]
  with NameOfFile[F, Path]
  with MoveFile[F, Path, Path] {

  override def mkDir(dir: Path, name: String): F[Path] =
    if (Files.exists(dir.resolve(name))) {
      dir.resolve(name).pure[F]
    } else {
      Files.createDirectory(dir.resolve(name)).pure[F]
    }

  override def mkFile(dir: Path, name: String): F[Path] =
    if (Files.exists(dir.resolve(name))) {
      dir.resolve(name).pure[F]
    } else {
      Files.createFile(dir.resolve(name)).pure[F]
    }

  override def filesInDir(dir: Path): F[List[Path]] =
    Files.list(dir).iterator().asScala.toList.pure[F]

  override def nameOfFile(file: Path): F[String] =
    file.getFileName.toString.pure[F]

  override def moveFile(dir: Path, file: Path): F[Path] =
    Files.move(file, dir.resolve(file.getFileName)).pure[F]
}
class ConsolePathPrinter[F[_] : Applicative] extends Printer[F, Path] {
  override def printName(file: Path): F[Unit] = println(file.getFileName).pure[F]
}

object TypeClasses {
  def main(args: Array[String]): Unit = {
    implicit val fs: RealFileSystem[Id] = new RealFileSystem[Id]
    implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
    val program = new Program[Id, Path, Path]

    program.run(Paths.get("."))
  }
}