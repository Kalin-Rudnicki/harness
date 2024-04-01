package harness.endpoint.spec

import cats.data.NonEmptyList
import harness.deriving.*
import harness.schema.JsonSchema
import harness.web.HttpCode
import zio.json.*

final case class errorCode(code: HttpCode) extends scala.annotation.Annotation
final case class errorExamples[A](ex0: A, exN: A*) extends scala.annotation.Annotation {
  def toNel: NonEmptyList[A] = NonEmptyList(ex0, exN.toList)
}

sealed trait ErrorSchema[A] {

  final val allCodes: Set[HttpCode] =
    this match {
      case ErrorSchema.ForProduct(_, code, _, _) => Set(code)
      case ErrorSchema.ForSum(schema, children)  => children.map(_.code).toSet
      case ErrorSchema.ForNothing                => Set.empty
    }

  // TODO (KR) : support pretty
  final def encode(a: A): (HttpCode, String) =
    this match {
      case ErrorSchema.ForProduct(_, code, schema, _) => (code, a.toJson(using schema.codec.encoder))
      case ErrorSchema.ForSum(schema, children) =>
        (
          children.find(_.tag.closestClass.isInstance(a)).fold(HttpCode.`500`)(_.code),
          a.toJson(using schema.codec.encoder),
        )
      case ErrorSchema.ForNothing => throw new RuntimeException("ErrorSchema.ForNothing")
    }

  final def decode(code: HttpCode, error: String): Option[Either[String, A]] =
    Option.when(allCodes.contains(code)) {
      this match {
        case ErrorSchema.ForProduct(_, _, schema, _) => schema.decode(error)
        case ErrorSchema.ForSum(schema, _)           => schema.decode(error)
        case ErrorSchema.ForNothing                  => throw new RuntimeException("ErrorSchema.ForNothing")
      }
    }

}
object ErrorSchema extends K0.Derivable[ErrorSchema.ForJson] {

  case object ForNothing extends ErrorSchema[Nothing]

  sealed trait ForJson[A] extends ErrorSchema[A]

  final case class ForProduct[A] private[ErrorSchema] (
      tag: zio.Tag[A],
      code: HttpCode,
      schema: JsonSchema[A],
      examples: NonEmptyList[A],
  ) extends ForJson[A]

  final case class ForSum[A] private[ErrorSchema] (
      schema: JsonSchema[A],
      children: List[ForProduct[A]],
  ) extends ForJson[A]

  // =====|  |=====

  override inline implicit def genProduct[A](implicit m: K0.ProductGeneric[A]): Derived[ErrorSchema.ForJson[A]] =
    Derived {
      ErrorSchema.ForProduct[A](
        zio.Tag[A],
        RequiredAnnotation[A, errorCode].annotation.code,
        JsonSchema.genProduct[A].derived,
        RequiredAnnotation[A, errorExamples[A]].annotation.toNel,
      )
    }

  override inline implicit def genSum[A](implicit m: K0.SumGeneric[A]): Derived[ErrorSchema.ForJson[A]] =
    Derived {
      ErrorSchema.ForSum[A](
        JsonSchema.genSum[A].derived,
        K0.SumInstances.of[A, ErrorSchema.ForJson].narrow[ErrorSchema.ForProduct].children.asInstanceOf[List[ForProduct[A]]],
      )
    }

}
