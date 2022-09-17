package harness.web.server

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.core.*
import harness.zio.*
import scala.annotation.tailrec
import zio.*

trait RouteMatcher[+O] private[server] { self =>

  final def /[O2](other: RouteMatcher[O2])(implicit zip: Zippable[O, O2]): RouteMatcher[zip.Out] =
    (method, path) =>
      self.routeInternal(method, path).flatMap { (remainingPath, o) =>
        other.routeInternal(method, remainingPath).map(zip.zip(o, _))
      }

  final def map[O2](f: O => O2): RouteMatcher[O2] =
    self.routeInternal(_, _).map(f)

  final def implement[R](f: O => SHRION[ServerEnv & RequestEnv & R, HttpResponse]): Route[R] =
    (method, path) =>
      self.routeInternal(method, path) match {
        case RouteMatcher.Result.Success(Nil, value) => f(value)
        case RouteMatcher.Result.Success(_, _)       => ZIO.succeed(HttpResponse.NotFound)
        case RouteMatcher.Result.Fail(errors)        => ZIO.fail(errors)
        case RouteMatcher.Result.NotFound            => ZIO.succeed(HttpResponse.NotFound)
      }

  def routeInternal(
      method: HttpMethod,
      path: List[String],
  ): RouteMatcher.Result[O]

}
object RouteMatcher {

  sealed trait Result[+O] {

    final def map[O2](f: O => O2): Result[O2] =
      this match {
        case Result.Success(remainingPath, value) => Result.Success(remainingPath, f(value))
        case fail @ Result.Fail(_)                => fail
        case Result.NotFound                      => Result.NotFound
      }

    final def flatMap[O2](f: (List[String], O) => Result[O2]): Result[O2] =
      this match {
        case Result.Success(remainingPath, value) => f(remainingPath, value)
        case fail @ Result.Fail(_)                => fail
        case Result.NotFound                      => Result.NotFound
      }

  }
  object Result {
    final case class Success[+O](remainingPath: List[String], value: O) extends Result[O]
    final case class Fail(errors: NonEmptyList[HError]) extends Result[Nothing]
    case object NotFound extends Result[Nothing]
  }

  // =====| Builders |=====

  final class *[A] private (implicit val decoder: StringDecoder[A])
  object * {
    def apply[A: StringDecoder]: *[A] = new *[A]
  }

  object **

}
