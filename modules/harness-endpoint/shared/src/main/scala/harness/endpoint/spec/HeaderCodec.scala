package harness.endpoint.spec

import cats.data.NonEmptyList
import cats.syntax.either.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import harness.core.{EitherN, Zip}
import harness.endpoint.error.DecodingFailure
import harness.schema.*
import scala.annotation.tailrec

trait HeaderCodec[A] { self =>

  lazy val schemas: NonEmptyList[List[GenericHeaderSchema]]
  final lazy val sources: NonEmptyList[List[SchemaSource]] = schemas.map(_.map(_.source))

  // TODO (KR) : Either
  def decodeInternal(headers: Map[String, List[String]], cookies: Map[String, String]): Either[DecodingFailure, HeaderCodec.Decoded[A]]
  final def decode(headers: Map[String, List[String]], cookies: Map[String, String]): Either[DecodingFailure, A] = self.decodeInternal(headers, cookies).map(_.value)
  def encode(o: A): (Map[String, List[String]], Map[String, String])

  final def ++[B](that: HeaderCodec[B])(implicit z: Zip[A, B]): HeaderCodec[z.Out] = HeaderCodec.And(self, that, z)
  final def imap[B](to: A => B, from: B => A): HeaderCodec[B] = HeaderCodec.IMap(self, to, from)
  final def imapDecode[B](to: A => Either[String, B], from: B => A): HeaderCodec[B] = HeaderCodec.IMapDecode(self, to, from)

}
object HeaderCodec {

  final case class Decoded[+A](value: A, from: List[SchemaSource]) {
    def map[B](f: A => B): Decoded[B] = Decoded(f(value), from)
    def mapDecode[B](f: A => Either[String, B]): Either[DecodingFailure, Decoded[B]] =
      f(value) match {
        case Right(value) => Decoded(value, from).asRight
        case Left(err)    => DecodingFailure(from, DecodingFailure.Cause.DecodeFail(err)).asLeft
      }
  }

  case object Empty extends HeaderCodec[Unit] {
    override lazy val schemas: NonEmptyList[List[GenericHeaderSchema]] = NonEmptyList.one(Nil)
    override def decodeInternal(headers: Map[String, List[String]], cookies: Map[String, String]): Either[DecodingFailure, Decoded[Unit]] = Decoded((), Nil).asRight
    override def encode(o: Unit): (Map[String, List[String]], Map[String, String]) = (Map.empty, Map.empty)
  }

  sealed trait WithKey[A, B] extends HeaderCodec[B] {
    val key: String
    val schema: Schema[A]
    val requirement: SchemaRequirement
    protected val makeSchema: (SchemaRequirement, String, Schema[A]) => GenericHeaderSchema
    protected val getValues: (Map[String, List[String]], Map[String, String]) => List[String]
    protected val parseValues: List[String] => Either[DecodingFailure, B]

    protected final lazy val upperKey: String = key.toUpperCase
    protected final lazy val schemaSource: SchemaSource = headerSchema.source
    protected final lazy val headerSchema: GenericHeaderSchema = makeSchema(requirement, key, schema)

    override final lazy val schemas: NonEmptyList[List[GenericHeaderSchema]] = NonEmptyList.one(headerSchema :: Nil)

    protected final def decodeSingle(v: String): Either[DecodingFailure, A] =
      schema.decode(v).leftMap(e => DecodingFailure(schemaSource :: Nil, DecodingFailure.Cause.DecodeFail(e)))

    protected final def fail(cause: DecodingFailure.Cause): Either[DecodingFailure, Nothing] = DecodingFailure(schemaSource :: Nil, cause).asLeft

    override def decodeInternal(headers: Map[String, List[String]], cookies: Map[String, String]): Either[DecodingFailure, Decoded[B]] =
      if (headers.contains(upperKey) && cookies.contains(upperKey))
        fail(DecodingFailure.Cause.ProvidedAsHeaderAndCookie)
      else
        parseValues(getValues(headers, cookies)) match {
          case Right(value) => Decoded(value, schemaSource :: Nil).asRight
          case Left(error)  => error.asLeft
        }

  }

  sealed trait HeaderWithKey[A, B] extends WithKey[A, B] {
    override protected val makeSchema: (SchemaRequirement, String, Schema[A]) => GenericHeaderSchema = HeaderSchema(_, _, _)
    override protected val getValues: (Map[String, List[String]], Map[String, String]) => List[String] = (h, _) => h.getOrElse(upperKey, Nil)
  }

  sealed trait CookieWithKey[A, B] extends WithKey[A, B] {
    override protected val makeSchema: (SchemaRequirement, String, Schema[A]) => GenericHeaderSchema = CookieSchema(_, _, _)
    override protected val getValues: (Map[String, List[String]], Map[String, String]) => List[String] = (_, c) => c.get(upperKey).toList
  }

  sealed trait HeaderOrCookieWithKey[A, B] extends WithKey[A, B] {
    override protected val makeSchema: (SchemaRequirement, String, Schema[A]) => GenericHeaderSchema = HeaderOrCookieSchema(_, _, _)
    override protected val getValues: (Map[String, List[String]], Map[String, String]) => List[String] = (h, c) => h.getOrElse(upperKey, Nil) ::: c.get(upperKey).toList
  }

  sealed trait Required[A] extends HeaderCodec[A] { self: WithKey[A, A] =>
    override val requirement: SchemaRequirement = SchemaRequirement.Required
    override protected val parseValues: List[String] => Either[DecodingFailure, A] = {
      case value :: Nil => decodeSingle(value)
      case Nil          => fail(DecodingFailure.Cause.MissingRequired)
      case _            => fail(DecodingFailure.Cause.DoesNotAcceptMultiple)
    }
  }

  sealed trait Optional[A] extends HeaderCodec[Option[A]] { self: WithKey[A, Option[A]] =>
    override val requirement: SchemaRequirement = SchemaRequirement.Optional
    override protected val parseValues: List[String] => Either[DecodingFailure, Option[A]] = {
      case value :: Nil => decodeSingle(value).map(_.some)
      case Nil          => None.asRight
      case _            => fail(DecodingFailure.Cause.DoesNotAcceptMultiple)
    }
  }

  sealed trait Many[A] extends HeaderCodec[List[A]] { self: WithKey[A, List[A]] =>
    override val requirement: SchemaRequirement = SchemaRequirement.Many
    override protected val parseValues: List[String] => Either[DecodingFailure, List[A]] =
      _.traverse(decodeSingle)
  }

  sealed trait ManyNonEmpty[A] extends HeaderCodec[NonEmptyList[A]] { self: WithKey[A, NonEmptyList[A]] =>
    override val requirement: SchemaRequirement = SchemaRequirement.ManyNonEmpty
    override protected val parseValues: List[String] => Either[DecodingFailure, NonEmptyList[A]] = {
      case head :: tail => NonEmptyList(head, tail).traverse(decodeSingle)
      case _            => fail(DecodingFailure.Cause.MissingRequired)
    }
  }

  // --- Header ---

  final case class HeaderRequired[A](key: String, schema: Schema[A]) extends HeaderCodec.HeaderWithKey[A, A] with Required[A] {
    override def encode(o: A): (Map[String, List[String]], Map[String, String]) =
      (Map(key -> (schema.encode(o) :: Nil)), Map.empty)
  }
  object HeaderRequired {
    def apply[A](key: String)(implicit schema: RawSchema[A]): HeaderCodec.HeaderRequired[A] = new HeaderRequired(key, schema)
    def json[A](key: String)(implicit schema: JsonSchema[A]): HeaderCodec.HeaderRequired[A] = new HeaderRequired(key, schema)
  }

  final case class HeaderOptional[A](key: String, schema: Schema[A]) extends HeaderCodec.HeaderWithKey[A, Option[A]] with Optional[A] {
    override def encode(o: Option[A]): (Map[String, List[String]], Map[String, String]) =
      o match {
        case Some(o) => (Map(key -> (schema.encode(o) :: Nil)), Map.empty)
        case None    => (Map.empty, Map.empty)
      }
  }
  object HeaderOptional {
    def apply[A](key: String)(implicit schema: RawSchema[A]): HeaderCodec.HeaderOptional[A] = new HeaderOptional(key, schema)
    def json[A](key: String)(implicit schema: JsonSchema[A]): HeaderCodec.HeaderOptional[A] = new HeaderOptional(key, schema)
  }

  final case class HeaderMany[A](key: String, schema: Schema[A]) extends HeaderCodec.HeaderWithKey[A, List[A]] with Many[A] {
    override def encode(o: List[A]): (Map[String, List[String]], Map[String, String]) =
      o match {
        case Nil => (Map.empty, Map.empty)
        case _   => (Map(key -> o.map(schema.encode)), Map.empty)
      }
  }
  object HeaderMany {
    def apply[A](key: String)(implicit schema: RawSchema[A]): HeaderCodec.HeaderMany[A] = new HeaderMany(key, schema)
    def json[A](key: String)(implicit schema: JsonSchema[A]): HeaderCodec.HeaderMany[A] = new HeaderMany(key, schema)
  }

  final case class HeaderManyNonEmpty[A](key: String, schema: Schema[A]) extends HeaderCodec.HeaderWithKey[A, NonEmptyList[A]] with ManyNonEmpty[A] {
    override def encode(o: NonEmptyList[A]): (Map[String, List[String]], Map[String, String]) =
      (Map(key -> o.toList.map(schema.encode)), Map.empty)
  }
  object HeaderManyNonEmpty {
    def apply[A](key: String)(implicit schema: RawSchema[A]): HeaderCodec.HeaderManyNonEmpty[A] = new HeaderManyNonEmpty(key, schema)
    def json[A](key: String)(implicit schema: JsonSchema[A]): HeaderCodec.HeaderManyNonEmpty[A] = new HeaderManyNonEmpty(key, schema)
  }

  // --- Cookie ---

  final case class CookieRequired[A](key: String, schema: Schema[A]) extends HeaderCodec.CookieWithKey[A, A] with Required[A] {
    override def encode(o: A): (Map[String, List[String]], Map[String, String]) =
      (Map.empty, Map(key -> schema.encode(o)))
  }
  object CookieRequired {
    def apply[A](key: String)(implicit schema: RawSchema[A]): HeaderCodec.CookieRequired[A] = new CookieRequired(key, schema)
    def json[A](key: String)(implicit schema: JsonSchema[A]): HeaderCodec.CookieRequired[A] = new CookieRequired(key, schema)
  }

  final case class CookieOptional[A](key: String, schema: Schema[A]) extends HeaderCodec.CookieWithKey[A, Option[A]] with Optional[A] {
    override def encode(o: Option[A]): (Map[String, List[String]], Map[String, String]) =
      o match {
        case Some(o) => (Map.empty, Map(key -> schema.encode(o)))
        case None    => (Map.empty, Map.empty)
      }
  }
  object CookieOptional {
    def apply[A](key: String)(implicit schema: RawSchema[A]): HeaderCodec.CookieOptional[A] = new CookieOptional(key, schema)
    def json[A](key: String)(implicit schema: JsonSchema[A]): HeaderCodec.CookieOptional[A] = new CookieOptional(key, schema)
  }

  // --- Header or Cookie ---

  final case class HeaderOrCookieRequired[A](key: String, schema: Schema[A]) extends HeaderCodec.HeaderOrCookieWithKey[A, A] with Required[A] {
    override def encode(o: A): (Map[String, List[String]], Map[String, String]) =
      (Map.empty, Map(key -> schema.encode(o)))
  }
  object HeaderOrCookieRequired {
    def apply[A](key: String)(implicit schema: RawSchema[A]): HeaderCodec.HeaderOrCookieRequired[A] = new HeaderOrCookieRequired(key, schema)
    def json[A](key: String)(implicit schema: JsonSchema[A]): HeaderCodec.HeaderOrCookieRequired[A] = new HeaderOrCookieRequired(key, schema)
  }

  final case class HeaderOrCookieOptional[A](key: String, schema: Schema[A]) extends HeaderCodec.HeaderOrCookieWithKey[A, Option[A]] with Optional[A] {
    override def encode(o: Option[A]): (Map[String, List[String]], Map[String, String]) =
      o match {
        case Some(o) => (Map.empty, Map(key -> schema.encode(o)))
        case None    => (Map.empty, Map.empty)
      }
  }
  object HeaderOrCookieOptional {
    def apply[A](key: String)(implicit schema: RawSchema[A]): HeaderCodec.HeaderOrCookieOptional[A] = new HeaderOrCookieOptional(key, schema)
    def json[A](key: String)(implicit schema: JsonSchema[A]): HeaderCodec.HeaderOrCookieOptional[A] = new HeaderOrCookieOptional(key, schema)
  }

  // --- Other ---

  final case class IMap[A, B](parent: HeaderCodec[A], to: A => B, from: B => A) extends HeaderCodec[B] {

    override lazy val schemas: NonEmptyList[List[GenericHeaderSchema]] = parent.schemas

    override def decodeInternal(headers: Map[String, List[String]], cookies: Map[String, String]): Either[DecodingFailure, Decoded[B]] =
      parent.decodeInternal(headers, cookies).map { _.map(to) }

    override def encode(o: B): (Map[String, List[String]], Map[String, String]) =
      parent.encode(from(o))

  }

  final case class IMapDecode[A, B](parent: HeaderCodec[A], to: A => Either[String, B], from: B => A) extends HeaderCodec[B] {

    override lazy val schemas: NonEmptyList[List[GenericHeaderSchema]] = parent.schemas

    override def decodeInternal(headers: Map[String, List[String]], cookies: Map[String, String]): Either[DecodingFailure, Decoded[B]] =
      parent.decodeInternal(headers, cookies).flatMap { _.mapDecode(to) }

    override def encode(o: B): (Map[String, List[String]], Map[String, String]) =
      parent.encode(from(o))

  }

  final case class OneOf[A](
      options: NonEmptyList[HeaderCodec[? <: A]],
      enc: A => (Map[String, List[String]], Map[String, String]),
  ) extends HeaderCodec[A] {

    override lazy val schemas: NonEmptyList[List[GenericHeaderSchema]] = options.flatMap(_.schemas)

    override def decodeInternal(headers: Map[String, List[String]], cookies: Map[String, String]): Either[DecodingFailure, Decoded[A]] = {
      @tailrec
      def loop(
          options: List[HeaderCodec[? <: A]],
          errors: List[DecodingFailure],
      ): Either[DecodingFailure, Decoded[A]] =
        options match {
          case head :: tail =>
            head.decodeInternal(headers, cookies) match {
              case res @ Right(_) => res
              case Left(err)      => loop(tail, err :: errors)
            }
          case Nil =>
            DecodingFailure.or(NonEmptyList.fromListUnsafe(errors.reverse)).asLeft
        }

      loop(options.toList, Nil)
    }

    override def encode(o: A): (Map[String, List[String]], Map[String, String]) = enc(o)

  }
  object OneOf {

    def apply[T]: Builder[T] = new Builder[T]

    final class Builder[T] {

      // TODO (KR) : add larger `apply` methods if needed

      def apply[A <: T, B <: T, C <: T, D <: T, E <: T](
          a: HeaderCodec[A],
          b: HeaderCodec[B],
          c: HeaderCodec[C],
          d: HeaderCodec[D],
          e: HeaderCodec[E],
      )(split: T => EitherN.Either5[A, B, C, D, E]): HeaderCodec.OneOf[T] =
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
          a: HeaderCodec[A],
          b: HeaderCodec[B],
          c: HeaderCodec[C],
          d: HeaderCodec[D],
      )(split: T => EitherN.Either4[A, B, C, D]): HeaderCodec.OneOf[T] =
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
          a: HeaderCodec[A],
          b: HeaderCodec[B],
          c: HeaderCodec[C],
      )(split: T => EitherN.Either3[A, B, C]): HeaderCodec.OneOf[T] =
        new OneOf[T](
          NonEmptyList.of(a, b, c),
          split(_) match {
            case EitherN._1(value) => a.encode(value)
            case EitherN._2(value) => b.encode(value)
            case EitherN._3(value) => c.encode(value)
          },
        )

      def apply[A <: T, B <: T](
          a: HeaderCodec[A],
          b: HeaderCodec[B],
      )(split: T => EitherN.Either2[A, B]): HeaderCodec.OneOf[T] =
        new OneOf[T](
          NonEmptyList.of(a, b),
          split(_) match {
            case EitherN._1(value) => a.encode(value)
            case EitherN._2(value) => b.encode(value)
          },
        )

    }

  }

  final case class And[A, B, O](a: HeaderCodec[A], b: HeaderCodec[B], z: Zip.Out[A, B, O]) extends HeaderCodec[O] {

    override lazy val schemas: NonEmptyList[List[GenericHeaderSchema]] =
      for {
        aSchemas <- a.schemas
        bSchemas <- b.schemas
      } yield aSchemas ::: bSchemas

    override def decodeInternal(headers: Map[String, List[String]], cookies: Map[String, String]): Either[DecodingFailure, Decoded[O]] =
      for {
        aValue <- a.decodeInternal(headers, cookies)
        bValue <- b.decodeInternal(headers, cookies)
      } yield Decoded(z.zip(aValue.value, bValue.value), aValue.from ::: bValue.from)

    override def encode(o: O): (Map[String, List[String]], Map[String, String]) = {
      val (aValue, bValue) = z.unzip(o)
      val (aHeaders, aCookies) = a.encode(aValue)
      val (bHeaders, bCookies) = b.encode(bValue)
      (aHeaders ++ bHeaders, aCookies ++ bCookies)
    }

  }

}
