package info.hjaem.bytecodeanalyzer

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.apache.commons.io.FileUtils
import java.io.{File, ByteArrayOutputStream}
import scala.jdk.CollectionConverters._

class JNISpec extends AnyFlatSpec with Matchers {

  val dir =
    new File(System.getProperty("user.dir")).getParentFile.getAbsolutePath +
      File.separator + "test" +
      File.separator + "jni" +
      File.separator

  val analyzeTests = List(
    (
      "Ptr",
      List("")
    ),
    (
      "PtrDf",
      List("main:18 (T, {}, {As(0)}) (double-free)")
    ),
    (
      "Box",
      List("")
    ),
    (
      "BoxUaf",
      List("main:24 (T, {}, {As(0)}) (use-after-free)")
    ),
    (
      "Point",
      List("")
    ),
    (
      "PointWrong",
      List(
        "main:29 (T, {}, {As(0)}) (use-after-free)",
        "main:35 (T, {}, {As(0)}) (use-after-free)",
        "main:39 (T, {}, {As(0)}) (double-free)"
      )
    )
  )

  for ((f, results) <- analyzeTests) {
    f should "be analyzed" in {
      val pg = Program(dir + f + ".class")
      val summary = Summary.fromFile(dir + f + ".summary")
      val out = new ByteArrayOutputStream
      Console.withOut(out) {
        new Analyzer(pg, summary).run()
      }
      val res =
        new String(out.toByteArray)
          .split("\n")
          .toList
      res shouldEqual results
    }
  }
}
