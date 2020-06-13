package info.hjaem.bytecodeanalyzer

import play.api.libs.json._
import scala.io.Source

case class Summary(
  functions: Map[String, FunctionSummary]
)

case class FunctionSummary(
  params: List[ParamSummary],
  ret: List[String],
  env: Map[String, List[String]]
)

case class ParamSummary(
  name: String,
  used: Boolean,
  freed: Boolean,
)

object Summary {
  implicit val reads: Reads[Summary] = Json.reads[Summary]

  def fromFile(filename: String): Summary =
    Summary(
      Json.parse(Source.fromFile(filename).mkString)
        .as[Map[String, FunctionSummary]]
    )
}

object FunctionSummary {
  implicit val reads: Reads[FunctionSummary] = Json.reads[FunctionSummary]
}

object ParamSummary {
  implicit val reads: Reads[ParamSummary] = Json.reads[ParamSummary]
}
