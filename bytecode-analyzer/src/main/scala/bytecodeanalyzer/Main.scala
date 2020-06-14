package info.hjaem.bytecodeanalyzer

object Main {

  def main(args: Array[String]): Unit = args match {
    case Array(filename1, filename2) =>
      val pg = Program(filename1)
      val summary = Summary.fromFile(filename2)
      // println(pg)
      // Interpreter.run(pg)
      new Analyzer(pg, summary).run()

    case _ => println("Usage: sbt run [file] [file]")
  }

}
