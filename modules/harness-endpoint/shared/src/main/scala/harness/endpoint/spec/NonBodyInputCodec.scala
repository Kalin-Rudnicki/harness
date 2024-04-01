package harness.endpoint.spec

import cats.syntax.option.*
import harness.core.Enum
import harness.schema.Schema

final case class NonBodyInputCodec[Path, All](
    encode: All => (List[String], Map[String, List[String]], Map[String, List[String]], Map[String, String]),
    decodePath: List[String] => Option[Path],
    decodeAll: (Path, Map[String, List[String]], Map[String, List[String]], Map[String, String]) => NonBodyInputCodec.Result[All],
    pathSchemas: List[NonBodyInputCodec.PathSchema],
    querySchemas: List[NonBodyInputCodec.OtherSchema.QuerySchema],
    headerSchemas: List[NonBodyInputCodec.OtherSchema.HeaderBasedSchema],
) { self =>

  type PathT = Path
  
  def /:(constPath: String): NonBodyInputCodec[Path, All] =
    NonBodyInputCodec[Path, All](
      encode = { a =>
        val (path, queries, headers, cookies) = self.encode(a)
        (constPath :: path, queries, headers, cookies)
      },
      decodePath = {
        case `constPath` :: pathT => self.decodePath(pathT)
        case _                    => None
      },
      decodeAll = self.decodeAll,
      pathSchemas = NonBodyInputCodec.PathSchema.Const(constPath) :: self.pathSchemas,
      querySchemas = self.querySchemas,
      headerSchemas = self.headerSchemas,
    )

  def /:(constPaths: List[String]): NonBodyInputCodec[Path, All] =
    constPaths.foldRight(self)(_ /: _)

}
object NonBodyInputCodec {

  val empty: NonBodyInputCodec[Unit, Unit] =
    NonBodyInputCodec[Unit, Unit](
      encode = _ => (Nil, Map.empty, Map.empty, Map.empty),
      decodePath = {
        case Nil => ().some
        case _   => None
      },
      decodeAll = (a, _, _, _) => Result.Success(a),
      pathSchemas = Nil,
      querySchemas = Nil,
      headerSchemas = Nil,
    )

  sealed trait Result[+A] {

    final def map[B](f: A => B): Result[B] =
      this match {
        case Result.Success(value) => Result.Success(f(value))
        case fail: Result.Fail     => fail
      }

    final def flatMap[B](f: A => Result[B]): Result[B] =
      this match {
        case Result.Success(value) => f(value)
        case fail: Result.Fail     => fail
      }

  }
  object Result {

    final case class Success[+A](value: A) extends Result[A]

    final case class Fail(schema: OtherSchema, reason: FailureReason) extends Result[Nothing]

    inline def fromEither[A](inline schema: OtherSchema, inline either: Either[String, A]): Result[A] =
      either match {
        case Right(value) => Result.Success(value)
        case Left(error)  => Result.Fail(schema, FailureReason.DecodeFail(error))
      }

  }

  // =====|  |=====

  enum Requirement extends Enum[Requirement] { case Required, Optional, Many, ManyNonEmpty }
  object Requirement extends Enum.Companion[Requirement]

  sealed trait PathSchema
  object PathSchema {
    final case class Const(const: String) extends PathSchema
    final case class Param(name: String, schema: Schema[?]) extends PathSchema
    final case class Rest(name: String) extends PathSchema
    final case class RestNonEmpty(name: String) extends PathSchema
  }

  sealed trait OtherSchema {
    val key: String
    val schema: Schema[?]
  }
  object OtherSchema {
    final case class QuerySchema(key: String, schema: Schema[?], requirement: Requirement) extends OtherSchema

    sealed trait HeaderBasedSchema extends OtherSchema
    final case class HeaderSchema(key: String, schema: Schema[?], requirement: Requirement) extends HeaderBasedSchema
    final case class CookieSchema(key: String, schema: Schema[?], required: Boolean) extends HeaderBasedSchema
    final case class HeaderOrCookieSchema(key: String, schema: Schema[?], required: Boolean) extends HeaderBasedSchema
  }

  sealed trait FailureReason
  object FailureReason {
    case object Missing extends FailureReason
    case object DoesNotAcceptMultiple extends FailureReason
    final case class DecodeFail(error: String) extends FailureReason
  }

}
