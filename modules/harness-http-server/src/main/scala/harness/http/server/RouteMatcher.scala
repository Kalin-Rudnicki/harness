package harness.http.server

import cats.syntax.option.*
import harness.core.*
import harness.http.server.error.*
import harness.web.*
import harness.zio.*
import zio.*

trait RouteMatcher[+O] private[server] { self =>

  final def /[O2](other: RouteMatcher[O2])(implicit zip: Zippable[O, O2]): RouteMatcher[zip.Out] =
    (method, path) =>
      self.routeInternal(method, path).flatMap { (remainingPath, o) =>
        other.routeInternal(method, remainingPath).map(zip.zip(o, _))
      }

  final def map[O2](f: O => O2): RouteMatcher[O2] =
    self.routeInternal(_, _).map(f)

  final def implement[R, DomainError, ApiError](f: O => ZIO[HarnessEnv & BuiltInRequestEnv & R, DomainError, HttpResponse.Found])(implicit
      handler: ErrorHandler[DomainError, ApiError],
  ): Route[R] =
    (method, path) =>
      self.routeInternal(method, path) match {
        case RouteMatcher.Result.Success(Nil, value)   => Route.Result.Found(f(value), handler)
        case RouteMatcher.Result.Success(_, _)         => Route.Result.NotFound
        case RouteMatcher.Result.FailedToDecode(error) => Route.Result.Found(ZIO.fail(handler.convertDecodingFailure(error)), handler)
        case RouteMatcher.Result.NotFound              => Route.Result.NotFound
      }

  final def implementGenericError[R](f: O => RIO[HarnessEnv & BuiltInRequestEnv & R, HttpResponse.Found]): Route[R] =
    implement[R, Throwable, Throwable](f)(
      ErrorHandler(
        convertDecodingFailure = identity,
        convertUnexpectedError = identity,
        errorCode = _ => HttpCode.`500`,
        errorCodec = ErrorCodec.forThrowable,
        errorConverter = ErrorMapper.id,
        errorLogger = ErrorLogger.withJsonShow[Throwable](using EncodedThrowable.throwableJsonCodec.encoder).atLevel.error,
      ),
    )

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
        case fail @ Result.FailedToDecode(_)      => fail
        case Result.NotFound                      => Result.NotFound
      }

    final def flatMap[O2](f: (List[String], O) => Result[O2]): Result[O2] =
      this match {
        case Result.Success(remainingPath, value) => f(remainingPath, value)
        case fail @ Result.FailedToDecode(_)      => fail
        case Result.NotFound                      => Result.NotFound
      }

  }
  object Result {
    final case class Success[+O](remainingPath: List[String], value: O) extends Result[O]
    final case class FailedToDecode(decodingFailure: DecodingFailure) extends Result[Nothing]
    case object NotFound extends Result[Nothing]
  }

  // =====| Builders |=====

  final class *[A] private (implicit val decoder: StringDecoder[A])
  object * {
    def apply[A: StringDecoder]: *[A] = new *[A]
  }

  object **

}
