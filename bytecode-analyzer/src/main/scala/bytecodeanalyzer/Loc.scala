package info.hjaem.bytecodeanalyzer

import scala.util.parsing.combinator.RegexParsers

case class Loc(method: String, pc: Int) {
  override def toString: String = s"$method:$pc"
  def next: Loc = Loc(method, pc + 1)
}

sealed trait CLoc {
  override def toString: String = this match {
    case Null => "Null"
    case Var(n, f) => s"$n@$f"
    case Symbol(l) => s"\'$l"
    case Alloca(i) => s"Aa($i)"
    case Allocsite(i) => s"As($i)"
  }
}
case object Null extends CLoc
case class Var(name: String, func: String) extends CLoc
case class Symbol(loc: CLoc) extends CLoc
case class Alloca(id: Int) extends CLoc
case class Allocsite(id: Int) extends CLoc

object CLoc extends RegexParsers with Function[String, CLoc] {

  lazy val x: Parser[String] = "[0-9a-zA-Z_]+".r
  lazy val n: Parser[Int] = "[0-9]+".r ^^ (_.toInt)
  def wrap[T](p: => Parser[T]): Parser[T] = "(" ~> p <~ ")"

  lazy val l: Parser[CLoc] =
    "Null" ^^^ Null |
    "%" ~> x ~ wrap(x) ^^ { case n ~ f => Var(n, f) } |
    "\'" ~> l ^^ Symbol |
    "Alloca" ~> wrap(n) ^^ Alloca |
    "Allocsite" ~> wrap(n) ^^ Allocsite

  def apply(s: String): CLoc = parseAll(l, s) match {
    case Success(result, next) => result
    case _: NoSuccess => sys.error(s)
  }
}
