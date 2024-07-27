package harness.endpoint.error

import cats.data.NonEmptyList
import harness.endpoint.spec.SchemaSource
import harness.schema.*
import zio.json.*

sealed trait DecodingFailure extends Throwable {

  final def toSimples: NonEmptyList[DecodingFailure.Simple] = this match
    case simple: DecodingFailure.Simple => NonEmptyList.one(simple)
    case DecodingFailure.Or(simples)    => simples

}
object DecodingFailure {

  def apply(sources: List[SchemaSource], cause: Cause): DecodingFailure = DecodingFailure.Simple(sources, cause)
  def or(failures: NonEmptyList[DecodingFailure]): DecodingFailure =
    failures match {
      case NonEmptyList(fail, Nil) => fail
      case _                       => Or(failures.flatMap(_.toSimples))
    }
  def or(failure0: DecodingFailure, failureN: DecodingFailure*): DecodingFailure =
    or(NonEmptyList(failure0, failureN.toList))

  // =====|  |=====

  final case class Simple(
      sources: List[SchemaSource],
      cause: Cause,
  ) extends DecodingFailure {

    private def sourcesString: String =
      sources match {
        case head :: Nil => head.show
        case Nil         => "<no-source>"
        case _           => sources.map(_.show).mkString("[", ", ", "]")
      }

    override def getMessage: String = s"Decoding failure with $sourcesString : ${cause.show}"

  }
  object Simple {
    implicit val schema: JsonSchema[Simple] = JsonSchema.derived
  }

  final case class Or private[DecodingFailure] (simple: NonEmptyList[Simple]) extends DecodingFailure

  // =====|  |=====

  sealed abstract class Cause(final val show: String)
  object Cause {

    case object MissingRequired extends Cause("missing required value")
    case object DoesNotAcceptMultiple extends Cause("multiple values not allowed")

    case object MissingContentLength extends Cause("missing content length header")
    case object MultipleContentLengths extends Cause("received multiple content length headers")
    case object ContentLengthTooLong extends Cause("content length is too long")

    case object ProvidedAsHeaderAndCookie extends Cause("provided as header and cookie")

    final case class DecodeFail(message: String) extends Cause(message) // TODO (KR) : prefix?

    implicit val schema: JsonSchema[Cause] = JsonSchema.derived

  }

  implicit val schema: JsonSchema[DecodingFailure] =
    JsonSchema.nelSchema[Simple].timap(or, _.toSimples)

}
