package harness.endpoint.spec

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.{EitherN, Zip}
import harness.endpoint.error.DecodingFailure
import harness.schema.*
import scala.annotation.tailrec

trait QueryCodec[A] { self =>

  lazy val schemas: NonEmptyList[List[QuerySchema]]
  final lazy val sources: NonEmptyList[List[SchemaSource]] = schemas.map(_.map(_.source))

  // TODO (KR) : Either
  def decodeInternal(queryParams: Map[String, List[String]]): Either[DecodingFailure, QueryCodec.Decoded[A]]
  final def decode(queryParams: Map[String, List[String]]): Either[DecodingFailure, A] = self.decodeInternal(queryParams).map(_.value)
  def encode(o: A): Map[String, List[String]]

  final def ++[B](that: QueryCodec[B])(implicit z: Zip[A, B]): QueryCodec[z.Out] = QueryCodec.And(self, that, z)
  final def imap[B](to: A => B, from: B => A): QueryCodec[B] = QueryCodec.IMap(self, to, from)
  final def imapDecode[B](to: A => Either[String, B], from: B => A): QueryCodec[B] = QueryCodec.IMapDecode(self, to, from)

}
object QueryCodec {

  final case class Decoded[+A](value: A, from: List[SchemaSource]) {
    def map[B](f: A => B): Decoded[B] = Decoded(f(value), from)
    def mapDecode[B](f: A => Either[String, B]): Either[DecodingFailure, Decoded[B]] =
      f(value) match {
        case Right(value) => Decoded(value, from).asRight
        case Left(err)    => DecodingFailure(from, DecodingFailure.Cause.DecodeFail(err)).asLeft
      }
  }

  case object Empty extends QueryCodec[Unit] {
    override lazy val schemas: NonEmptyList[List[QuerySchema]] = NonEmptyList.one(Nil)
    override def decodeInternal(queryParams: Map[String, List[String]]): Either[DecodingFailure, Decoded[Unit]] = Decoded((), Nil).asRight
    override def encode(o: Unit): Map[String, List[String]] = Map.empty
  }

  sealed trait WithKey[A, B] extends QueryCodec[B] {
    val key: String
    val schema: Schema[A]
    val requirement: SchemaRequirement

    protected final lazy val querySchema: QuerySchema = QuerySchema(requirement, key, schema)
    protected final lazy val schemaSource: SchemaSource = querySchema.source

    override final lazy val schemas: NonEmptyList[List[QuerySchema]] = NonEmptyList.one(querySchema :: Nil)

    protected final def decodeSingle(v: String): Either[DecodingFailure, A] =
      schema.decode(v).leftMap(e => DecodingFailure(schemaSource :: Nil, DecodingFailure.Cause.DecodeFail(e)))

    protected final def fail(cause: DecodingFailure.Cause): Either[DecodingFailure, Nothing] = DecodingFailure(schemaSource :: Nil, cause).asLeft

  }

  final case class Required[A](key: String, schema: Schema[A]) extends QueryCodec.WithKey[A, A] {

    override val requirement: SchemaRequirement = SchemaRequirement.Required

    override def decodeInternal(queryParams: Map[String, List[String]]): Either[DecodingFailure, Decoded[A]] =
      queryParams.getOrElse(key, Nil) match {
        case value :: Nil => decodeSingle(value).map(Decoded(_, schemaSource :: Nil))
        case Nil          => fail(DecodingFailure.Cause.MissingRequired)
        case _            => fail(DecodingFailure.Cause.DoesNotAcceptMultiple)
      }

    override def encode(o: A): Map[String, List[String]] =
      Map(key -> (schema.encode(o) :: Nil))

  }
  object Required {
    def apply[A](key: String)(implicit schema: RawSchema[A]): QueryCodec.Required[A] = new Required(key, schema)
    def json[A](key: String)(implicit schema: JsonSchema[A]): QueryCodec.Required[A] = new Required(key, schema)
  }

  final case class Optional[A](key: String, schema: Schema[A]) extends QueryCodec.WithKey[A, Option[A]] {

    override val requirement: SchemaRequirement = SchemaRequirement.Optional

    override def decodeInternal(queryParams: Map[String, List[String]]): Either[DecodingFailure, Decoded[Option[A]]] =
      queryParams.getOrElse(key, Nil) match {
        case value :: Nil => decodeSingle(value).map(v => Decoded(v.some, schemaSource :: Nil))
        case Nil          => Decoded(None, schemaSource :: Nil).asRight // TODO (KR) : should source be included here?
        case _            => fail(DecodingFailure.Cause.DoesNotAcceptMultiple)
      }

    override def encode(o: Option[A]): Map[String, List[String]] =
      o match {
        case Some(value) => Map(key -> (schema.encode(value) :: Nil))
        case None        => Map.empty
      }

  }
  object Optional {
    def apply[A](key: String)(implicit schema: RawSchema[A]): QueryCodec.Optional[A] = new Optional(key, schema)
    def json[A](key: String)(implicit schema: JsonSchema[A]): QueryCodec.Optional[A] = new Optional(key, schema)
  }

  final case class Many[A](key: String, schema: Schema[A]) extends QueryCodec.WithKey[A, List[A]] {

    override val requirement: SchemaRequirement = SchemaRequirement.Many

    override def decodeInternal(queryParams: Map[String, List[String]]): Either[DecodingFailure, Decoded[List[A]]] =
      queryParams.getOrElse(key, Nil).traverse(decodeSingle).map(Decoded(_, schemaSource :: Nil))

    override def encode(o: List[A]): Map[String, List[String]] =
      o match {
        case Nil => Map.empty
        case _   => Map(key -> o.map(schema.encode))
      }

  }
  object Many {
    def apply[A](key: String)(implicit schema: RawSchema[A]): QueryCodec.Many[A] = new Many(key, schema)
    def json[A](key: String)(implicit schema: JsonSchema[A]): QueryCodec.Many[A] = new Many(key, schema)
  }

  final case class ManyNonEmpty[A](key: String, schema: Schema[A]) extends QueryCodec.WithKey[A, NonEmptyList[A]] {

    override val requirement: SchemaRequirement = SchemaRequirement.ManyNonEmpty

    override def decodeInternal(queryParams: Map[String, List[String]]): Either[DecodingFailure, Decoded[NonEmptyList[A]]] =
      queryParams.getOrElse(key, Nil) match {
        case head :: tail => NonEmptyList(head, tail).traverse(decodeSingle).map(Decoded(_, schemaSource :: Nil))
        case Nil          => fail(DecodingFailure.Cause.MissingRequired)
      }

    override def encode(o: NonEmptyList[A]): Map[String, List[String]] =
      Map(key -> o.toList.map(schema.encode))

  }
  object ManyNonEmpty {
    def apply[A](key: String)(implicit schema: RawSchema[A]): QueryCodec.ManyNonEmpty[A] = new ManyNonEmpty(key, schema)
    def json[A](key: String)(implicit schema: JsonSchema[A]): QueryCodec.ManyNonEmpty[A] = new ManyNonEmpty(key, schema)
  }

  final case class IMap[A, B](parent: QueryCodec[A], to: A => B, from: B => A) extends QueryCodec[B] {

    override lazy val schemas: NonEmptyList[List[QuerySchema]] = parent.schemas

    override def decodeInternal(queryParams: Map[String, List[String]]): Either[DecodingFailure, Decoded[B]] =
      parent.decodeInternal(queryParams).map { _.map(to) }

    override def encode(o: B): Map[String, List[String]] =
      parent.encode(from(o))

  }

  final case class IMapDecode[A, B](parent: QueryCodec[A], to: A => Either[String, B], from: B => A) extends QueryCodec[B] {

    override lazy val schemas: NonEmptyList[List[QuerySchema]] = parent.schemas

    override def decodeInternal(queryParams: Map[String, List[String]]): Either[DecodingFailure, Decoded[B]] =
      parent.decodeInternal(queryParams).flatMap { _.mapDecode(to) }

    override def encode(o: B): Map[String, List[String]] =
      parent.encode(from(o))

  }

  final case class OneOf[A](
      options: NonEmptyList[QueryCodec[? <: A]],
      enc: A => Map[String, List[String]],
  ) extends QueryCodec[A] {

    override lazy val schemas: NonEmptyList[List[QuerySchema]] = options.flatMap(_.schemas)

    override def decodeInternal(queryParams: Map[String, List[String]]): Either[DecodingFailure, Decoded[A]] = {
      @tailrec
      def loop(
          options: List[QueryCodec[? <: A]],
          errors: List[DecodingFailure],
      ): Either[DecodingFailure, Decoded[A]] =
        options match {
          case head :: tail =>
            head.decodeInternal(queryParams) match {
              case res @ Right(_) => res
              case Left(err)      => loop(tail, err :: errors)
            }
          case Nil =>
            DecodingFailure.or(NonEmptyList.fromListUnsafe(errors.reverse)).asLeft
        }

      loop(options.toList, Nil)
    }

    override def encode(o: A): Map[String, List[String]] = enc(o)

  }
  object OneOf {

    def apply[T]: Builder[T] = new Builder[T]

    final class Builder[T] {

      // TODO (KR) : add larger `apply` methods if needed

      def apply[A <: T, B <: T, C <: T, D <: T, E <: T](
          a: QueryCodec[A],
          b: QueryCodec[B],
          c: QueryCodec[C],
          d: QueryCodec[D],
          e: QueryCodec[E],
      )(split: T => EitherN.Either5[A, B, C, D, E]): QueryCodec.OneOf[T] =
        new OneOf[T](
          NonEmptyList.of(a, b, c, d, e),
          split(_) match {
            case EitherN._1(value) => a.encode(value)
            case EitherN._2(value) => b.encode(value)
            case EitherN._3(value) => c.encode(value)
            case EitherN._4(value) => d.encode(value)
            case EitherN._5(value) => e.encode(value)
          },
        )

      def apply[A <: T, B <: T, C <: T, D <: T](
          a: QueryCodec[A],
          b: QueryCodec[B],
          c: QueryCodec[C],
          d: QueryCodec[D],
      )(split: T => EitherN.Either4[A, B, C, D]): QueryCodec.OneOf[T] =
        new OneOf[T](
          NonEmptyList.of(a, b, c, d),
          split(_) match {
            case EitherN._1(value) => a.encode(value)
            case EitherN._2(value) => b.encode(value)
            case EitherN._3(value) => c.encode(value)
            case EitherN._4(value) => d.encode(value)
          },
        )

      def apply[A <: T, B <: T, C <: T](
          a: QueryCodec[A],
          b: QueryCodec[B],
          c: QueryCodec[C],
      )(split: T => EitherN.Either3[A, B, C]): QueryCodec.OneOf[T] =
        new OneOf[T](
          NonEmptyList.of(a, b, c),
          split(_) match {
            case EitherN._1(value) => a.encode(value)
            case EitherN._2(value) => b.encode(value)
            case EitherN._3(value) => c.encode(value)
          },
        )

      def apply[A <: T, B <: T](
          a: QueryCodec[A],
          b: QueryCodec[B],
      )(split: T => EitherN.Either2[A, B]): QueryCodec.OneOf[T] =
        new OneOf[T](
          NonEmptyList.of(a, b),
          split(_) match {
            case EitherN._1(value) => a.encode(value)
            case EitherN._2(value) => b.encode(value)
          },
        )

    }

  }

  final case class And[A, B, O](a: QueryCodec[A], b: QueryCodec[B], z: Zip.Out[A, B, O]) extends QueryCodec[O] {

    override lazy val schemas: NonEmptyList[List[QuerySchema]] =
      for {
        aSchemas <- a.schemas
        bSchemas <- b.schemas
      } yield aSchemas ::: bSchemas

    override def decodeInternal(queryParams: Map[String, List[String]]): Either[DecodingFailure, Decoded[O]] =
      for {
        aValue <- a.decodeInternal(queryParams)
        bValue <- b.decodeInternal(queryParams)
      } yield Decoded(z.zip(aValue.value, bValue.value), aValue.from ::: bValue.from)

    override def encode(o: O): Map[String, List[String]] = {
      val (aValue, bValue) = z.unzip(o)
      a.encode(aValue) ++ b.encode(bValue)
    }

  }

}
