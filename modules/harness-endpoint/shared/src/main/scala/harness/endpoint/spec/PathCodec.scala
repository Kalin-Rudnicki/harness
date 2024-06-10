package harness.endpoint.spec

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.core.{EitherN, Zip}
import harness.schema.*
import scala.annotation.tailrec

sealed trait PathCodec[A] { self =>

  val schemas: NonEmptyList[List[PathSchema]]

  protected def decodePathInternal(paths: List[String]): Option[(A, List[String])]
  def encodePath(o: A): List[String]

  final def decodePath(paths: List[String]): Option[A] =
    self.decodePathInternal(paths) match {
      case Some((a, Nil)) => a.some
      case _              => None
    }

  final def /[B](that: PathCodec[B])(implicit z: Zip[A, B]): PathCodec[z.Out] = PathCodec.Then(self, that, z)
  final def /[B](p: String): PathCodec[A] = self / PathCodec.Const(p)
  final def imap[B](to: A => B, from: B => A): PathCodec[B] = PathCodec.IMap(self, to, from)
  final def imapDecode[B](to: A => Option[B], from: B => A): PathCodec[B] = PathCodec.IMapDecode(self, to, from)

}
object PathCodec {

  // =====|  |=====

  case object Empty extends PathCodec[Unit] {

    override val schemas: NonEmptyList[List[PathSchema]] = NonEmptyList.one(Nil)

    override def decodePathInternal(paths: List[String]): Option[(Unit, List[String])] = ((), paths).some

    override def encodePath(o: Unit): List[String] = Nil

  }

  final case class Const(p: String) extends PathCodec[Unit] {

    override val schemas: NonEmptyList[List[PathSchema]] = NonEmptyList.one(PathSchema.Const(p) :: Nil)

    override def decodePathInternal(paths: List[String]): Option[(Unit, List[String])] =
      paths match {
        case `p` :: rem => ((), rem).some
        case _          => None
      }

    override def encodePath(o: Unit): List[String] = p :: Nil

  }

  final case class Param[A](name: String, schema: Schema[A]) extends PathCodec[A] {

    override val schemas: NonEmptyList[List[PathSchema]] = NonEmptyList.one(PathSchema.Param(name, schema) :: Nil)

    override def decodePathInternal(paths: List[String]): Option[(A, List[String])] =
      paths match {
        case head :: tail =>
          schema.decode(head) match {
            case Right(value) => (value, tail).some
            case Left(_)      => None
          }
        case Nil =>
          None
      }

    override def encodePath(o: A): List[String] = schema.encode(o) :: Nil

  }
  object Param {
    def apply[A](name: String)(implicit schema: RawSchema[A]): PathCodec.Param[A] = new Param(name, schema)
    def json[A](name: String)(implicit schema: JsonSchema[A]): PathCodec.Param[A] = new Param(name, schema)
  }

  final case class Rest(name: String) extends PathCodec[List[String]] {

    override val schemas: NonEmptyList[List[PathSchema]] = NonEmptyList.one(PathSchema.Rest(name) :: Nil)

    override def decodePathInternal(paths: List[String]): Option[(List[String], List[String])] = (paths, Nil).some

    override def encodePath(o: List[String]): List[String] = o

  }

  final case class RestNonEmpty(name: String) extends PathCodec[NonEmptyList[String]] {

    override val schemas: NonEmptyList[List[PathSchema]] = NonEmptyList.one(PathSchema.RestNel(name) :: Nil)

    override def decodePathInternal(paths: List[String]): Option[(NonEmptyList[String], List[String])] =
      paths match {
        case head :: tail => (NonEmptyList(head, tail), Nil).some
        case Nil          => None
      }

    override def encodePath(o: NonEmptyList[String]): List[String] = o.toList

  }

  final case class IMap[A, B](parent: PathCodec[A], to: A => B, from: B => A) extends PathCodec[B] {

    // TODO (KR) : map underlying Schema?
    override val schemas: NonEmptyList[List[PathSchema]] = parent.schemas

    override def decodePathInternal(paths: List[String]): Option[(B, List[String])] =
      parent.decodePathInternal(paths).map { case (a, rest) => (to(a), rest) }

    override def encodePath(o: B): List[String] = parent.encodePath(from(o))

  }

  final case class IMapDecode[A, B](parent: PathCodec[A], to: A => Option[B], from: B => A) extends PathCodec[B] {

    // TODO (KR) : map underlying Schema?
    override val schemas: NonEmptyList[List[PathSchema]] = parent.schemas

    override def decodePathInternal(paths: List[String]): Option[(B, List[String])] =
      for {
        (a, rest) <- parent.decodePathInternal(paths)
        b <- to(a)
      } yield (b, rest)

    override def encodePath(o: B): List[String] = parent.encodePath(from(o))

  }

  final case class OneOf[A](
      options: NonEmptyList[PathCodec[? <: A]],
      enc: A => List[String],
  ) extends PathCodec[A] {

    private val optionsList: List[PathCodec[? <: A]] = options.toList

    override val schemas: NonEmptyList[List[PathSchema]] = options.flatMap(_.schemas)

    override def decodePathInternal(paths: List[String]): Option[(A, List[String])] = {
      @tailrec
      def loop(options: List[PathCodec[? <: A]]): Option[(A, List[String])] =
        options match {
          case head :: tail =>
            head.decodePathInternal(paths) match {
              case res @ Some(_) => res
              case None          => loop(tail)
            }
          case Nil =>
            None
        }

      loop(optionsList)
    }

    override def encodePath(o: A): List[String] = enc(o)

  }
  object OneOf {

    def apply[T]: Builder[T] = new Builder[T]

    final class Builder[T] {

      // TODO (KR) : add larger `apply` methods if needed

      def apply[A <: T, B <: T, C <: T, D <: T, E <: T](
          a: PathCodec[A],
          b: PathCodec[B],
          c: PathCodec[C],
          d: PathCodec[D],
          e: PathCodec[E],
      )(split: T => EitherN.Either5[A, B, C, D, E]): PathCodec.OneOf[T] =
        new OneOf[T](
          NonEmptyList.of(a, b, c, d, e),
          split(_) match {
            case EitherN._1(value) => a.encodePath(value)
            case EitherN._2(value) => b.encodePath(value)
            case EitherN._3(value) => c.encodePath(value)
            case EitherN._4(value) => d.encodePath(value)
            case EitherN._5(value) => e.encodePath(value)
          },
        )

      def apply[A <: T, B <: T, C <: T, D <: T](
          a: PathCodec[A],
          b: PathCodec[B],
          c: PathCodec[C],
          d: PathCodec[D],
      )(split: T => EitherN.Either4[A, B, C, D]): PathCodec.OneOf[T] =
        new OneOf[T](
          NonEmptyList.of(a, b, c, d),
          split(_) match {
            case EitherN._1(value) => a.encodePath(value)
            case EitherN._2(value) => b.encodePath(value)
            case EitherN._3(value) => c.encodePath(value)
            case EitherN._4(value) => d.encodePath(value)
          },
        )

      def apply[A <: T, B <: T, C <: T](
          a: PathCodec[A],
          b: PathCodec[B],
          c: PathCodec[C],
      )(split: T => EitherN.Either3[A, B, C]): PathCodec.OneOf[T] =
        new OneOf[T](
          NonEmptyList.of(a, b, c),
          split(_) match {
            case EitherN._1(value) => a.encodePath(value)
            case EitherN._2(value) => b.encodePath(value)
            case EitherN._3(value) => c.encodePath(value)
          },
        )

      def apply[A <: T, B <: T](
          a: PathCodec[A],
          b: PathCodec[B],
      )(split: T => EitherN.Either2[A, B]): PathCodec.OneOf[T] =
        new OneOf[T](
          NonEmptyList.of(a, b),
          split(_) match {
            case EitherN._1(value) => a.encodePath(value)
            case EitherN._2(value) => b.encodePath(value)
          },
        )

    }

  }

  final case class Then[A, B, O](a: PathCodec[A], b: PathCodec[B], z: Zip.Out[A, B, O]) extends PathCodec[O] {

    override val schemas: NonEmptyList[List[PathSchema]] =
      for {
        aSchemas <- a.schemas
        bSchemas <- b.schemas
      } yield aSchemas ::: bSchemas

    // didn't use `for comp` because I feel like matches would be more efficient?
    override def decodePathInternal(paths: List[String]): Option[(O, List[String])] =
      a.decodePathInternal(paths) match {
        case Some((aValue, aRem)) =>
          b.decodePathInternal(aRem) match {
            case Some((bValue, bRem)) => (z.zip(aValue, bValue), bRem).some
            case None                 => None
          }
        case None => None
      }

    override def encodePath(o: O): List[String] = {
      val (aValue, bValue) = z.unzip(o)
      a.encodePath(aValue) ::: b.encodePath(bValue)
    }

  }

}
