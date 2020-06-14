package info.hjaem.bytecodeanalyzer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.apache.commons.io.FileUtils
import java.io.{File, ByteArrayOutputStream}
import scala.jdk.CollectionConverters._

class JavaSpec extends AnyFlatSpec with Matchers {

  val dir =
    new File(System.getProperty("user.dir")).getParentFile.getAbsolutePath +
      File.separator + "test" +
      File.separator + "java" +
      File.separator

  val parseTests = List(
    ("Ex1.class", List("x"), Set("main", "get", "set")),
    ("Ex2.class", List("x"), Set("main", "get", "set")),
    ("Ex3.class", List("x"), Set("main", "get", "set")),
    ("Ex4.class", List("x"), Set("main", "get", "sum")),
    ("Ex5.class", List(), Set("main", "foo", "bar")),
    ("Ex6.class", List(), Set("main", "foo")),
    ("Ex7.class", List("x"), Set("main", "get", "set"))
  )

  for ((f, fields, methods) <- parseTests) {
    f should "be parsed" in {
      val pg = Program(dir + f)
      pg.fields shouldEqual fields
      pg.methods.keySet shouldEqual methods
    }
  }

  val interpTests = List(
    ("Ex1.class", List(0, 1)),
    ("Ex2.class", List(0, 1, 1)),
    ("Ex3.class", List(1, 2, 3, 4, 5)),
    ("Ex4.class", List(15)),
    ("Ex6.class", List(1, 2, 3, 6)),
    ("Ex7.class", List(1))
  )

  for ((f, integers) <- interpTests) {
    f should "be interpreted" in {
      val pg = Program(dir + f)
      val out = new ByteArrayOutputStream
      Console.withOut(out) {
        Interpreter.run(pg)
      }
      val results =
        new String(out.toByteArray)
          .split("\n")
          .toList
          .map(_.toInt)
      results shouldEqual integers
    }
  }

  val analyzeTests = List(
    (
      "Ex1.class",
      List(
        "main:9 ({0,1}, {}, {})",
        "main:18 ({0,1}, {}, {})"
      )
    ),
    (
      "Ex2.class",
      List(
        "main:9 ({0,1}, {}, {})",
        "main:18 ({0,1}, {}, {})",
        "main:27 ({0,1}, {}, {})"
      )
    ),
    (
      "Ex3.class",
      List("main:21 (T, {}, {})")
    ),
    (
      "Ex4.class",
      List("main:13 (T, {main:1}, {})")
    ),
    (
      "Ex5.class",
      List("")
    ),
    (
      "Ex6.class",
      List(
        "foo:3 ({1}, {}, {})",
        "foo:7 ({2}, {}, {})",
        "foo:11 ({3}, {}, {})",
        "main:12 ({6}, {}, {})"
      )
    ),
    (
      "Ex7.class",
      List("main:38 ({0,1,2}, {}, {})")
    )
  )

  for ((f, results) <- analyzeTests) {
    f should "be analyzed" in {
      val pg = Program(dir + f)
      val out = new ByteArrayOutputStream
      Console.withOut(out) {
        new Analyzer(pg).run()
      }
      val res =
        new String(out.toByteArray)
          .split("\n")
          .toList
      res shouldEqual results
    }
  }
}
