package info.hjaem.bytecodeanalyzer

import play.api.libs.json._
import play.api.libs.functional.syntax._
import scala.io.Source

case class Summary(
  functions: Map[String, FunctionSummary]
)

case class FunctionSummary(
  params: List[ParamSummary],
  ret: List[CLoc],
  env: Map[CLoc, List[CLoc]]
) {
  println(params)
  println(ret)
  println(
  env.filter{
    case (_: Symbol, _) | (_: Allocsite, _) => true
    case _ => false
  }
  )
  println()

}

case class ParamSummary(
  name: CLoc,
  used: Boolean,
  freed: Boolean,
)

object Summary {
  def fromFile(filename: String): Summary =
    Summary(
      Json.parse(Source.fromFile(filename).mkString)
        .as[Map[String, FunctionSummary]]
    )
}

object FunctionSummary {
  implicit val reads: Reads[FunctionSummary] =
    (
      (JsPath \ "params").read[List[ParamSummary]] and
      (JsPath \ "ret").read[List[String]].map(_.map(CLoc)) and
      (JsPath \ "env").read[Map[String, List[String]]].map(
        _.map{ case (k, l) => CLoc(k) -> l.map(CLoc) }
      )
    )(FunctionSummary.apply _)
}

object ParamSummary {
  implicit val reads: Reads[ParamSummary] =
    (
      (JsPath \ "name").read[String].map(CLoc) and
      (JsPath \ "used").read[Boolean] and
      (JsPath \ "freed").read[Boolean]
    )(ParamSummary.apply _)
}
