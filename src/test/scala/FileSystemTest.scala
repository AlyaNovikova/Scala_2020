import java.nio.file.{Files, Path, Paths}
import java.util.Comparator

import org.scalatest.matchers.should.Matchers
import org.scalatest.flatspec.AnyFlatSpec
import cats.Id


class FileSystemTest extends AnyFlatSpec with Matchers {
  val path: Path = Paths.get("./tmp")
  val dir: Path = Files.createDirectory(path)

  implicit val fileSystem: RealFileSystem[Id] = new RealFileSystem[Id]
  implicit val printer: ConsolePathPrinter[Id] = new ConsolePathPrinter[Id]
  val program = new Program[Id, Path, Path]

  program.run(path)

  Files.exists(path.resolve("test_dir")) shouldBe true
  Files.exists(path.resolve("test_dir/f")) shouldBe true
  Files.exists(path.resolve("test_dir/b"))shouldBe true

  Files.exists(path.resolve("test_dir/f/foo")) shouldBe true
  Files.exists(path.resolve("test_dir/b/bar")) shouldBe true
  Files.exists(path.resolve("test_dir/b/baz")) shouldBe true
  Files.isRegularFile(path.resolve("test_dir/f/foo")) shouldBe true
  Files.isRegularFile(path.resolve("test_dir/b/bar")) shouldBe true
  Files.isRegularFile(path.resolve("test_dir/b/baz")) shouldBe true

  Files.walk(path).sorted(Comparator.reverseOrder()).forEach(file => Files.deleteIfExists(file))
}
