package info.hjaem.bytecodeanalyzer

object Main {

  def main(args: Array[String]): Unit = args match {
    case Array(filename) =>
      val pg = Program(filename)
      println(pg)
      // Interpreter.run(pg)
      new Analyzer(pg).run()

    case _ => println("give a file name")
  }

}
