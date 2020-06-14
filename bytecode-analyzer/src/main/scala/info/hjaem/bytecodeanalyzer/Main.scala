package info.hjaem.bytecodeanalyzer

object Main {

  def main(args: Array[String]): Unit = args match {
    case Array("--java-parse", filename) =>
      val pg = Program(filename)
      println(pg)

    case Array("--java-run", filename) =>
      val pg = Program(filename)
      Interpreter.run(pg)

    case Array("--java-analyze", filename) =>
      val pg = Program(filename)
      new Analyzer(pg).run()

    case Array("--jni-analyze", filename1, filename2) =>
      val pg = Program(filename1)
      val summary = Summary.fromFile(filename2)
      new Analyzer(pg, summary).run()

    case _ =>
      println("Usage: sbt run --java-parse [file]")
      println("Usage: sbt run --java-run [file]")
      println("Usage: sbt run --java-analyze [file]")
      println("Usage: sbt run --jni-analyze [file] [file]")
  }

}
