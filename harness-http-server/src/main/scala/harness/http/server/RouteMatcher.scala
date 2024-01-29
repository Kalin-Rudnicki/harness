package harness.http.server

import cats.syntax.option.*
import harness.core.*
import harness.web.*
import harness.zio.*
import zio.*
import zio.json.*

trait RouteMatcher[+O] private[server] { self =>

  final def /[O2](other: RouteMatcher[O2])(implicit zip: Zippable[O, O2]): RouteMatcher[zip.Out] =
    (method, path) =>
      self.routeInternal(method, path).flatMap { (remainingPath, o) =>
        other.routeInternal(method, remainingPath).map(zip.zip(o, _))
      }

  final def map[O2](f: O => O2): RouteMatcher[O2] =
    self.routeInternal(_, _).map(f)

  final def implement[R](f: O => SHRIO[BuiltInRequestEnv & R, HttpResponse]): Route[R] =
    (method, path) =>
      self.routeInternal(method, path) match {
        case RouteMatcher.Result.Success(Nil, value) => f(value)
        case RouteMatcher.Result.Success(_, _)       => ZIO.succeed(HttpResponse.NotFound)
        case RouteMatcher.Result.Fail(hError)        => ZIO.fail(hError)
        case RouteMatcher.Result.NotFound            => ZIO.succeed(HttpResponse.NotFound)
      }
  final def implementOr[R, E](f: O => SHZIO[BuiltInRequestEnv & R, E, HttpResponse])(implicit errorHandler: RouteMatcher.ErrorHandler[E]): Route[R] =
    (method, path) =>
      self.routeInternal(method, path) match {
        case RouteMatcher.Result.Success(Nil, value) =>
          f(value).recoverFromErrorOrCauseZIO { hError =>
            val code = errorHandler.httpCode(hError.error)
            val mappedHError = hError.mapSameCause(errorHandler.showError)
            Logger.log
              .info(s"Resulted in expected error:\n${mappedHError.fullInternalMessage}")
              .as(errorHandler.modifyHttpResponse(HttpResponse(mappedHError.error, code), hError.error))
          }
        case RouteMatcher.Result.Success(_, _)        => ZIO.succeed(HttpResponse.NotFound)
        case RouteMatcher.Result.Fail(hError: HError) => ZIO.fail(hError)
        case RouteMatcher.Result.NotFound             => ZIO.succeed(HttpResponse.NotFound)
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
    final case class Fail(error: HError) extends Result[Nothing]
    case object NotFound extends Result[Nothing]
  }

  final case class ErrorHandler[-E](
      showError: E => String,
      httpCode: E => HttpCode,
      modifyHttpResponse: (HttpResponse.Found, E) => HttpResponse.Found,
  )
  object ErrorHandler {

    final case class Builder1[E](
        showError: E => String,
    ) {
      def withHttpCode(httpCode: E => HttpCode): ErrorHandler.Builder2[E] = ErrorHandler.Builder2(showError, httpCode)
      def with400HttpCode: ErrorHandler.Builder2[E] = withHttpCode(_ => HttpCode.`400`)
      def with500HttpCode: ErrorHandler.Builder2[E] = withHttpCode(_ => HttpCode.`500`)
    }

    final case class Builder2[E](
        showError: E => String,
        httpCode: E => HttpCode,
    ) {
      def withUnmodifiedResponse: ErrorHandler[E] = ErrorHandler[E](showError, httpCode, (response, _) => response)
      def withModifiedResponse(modifyHttpResponse: (HttpResponse.Found, E) => HttpResponse.Found): ErrorHandler[E] = ErrorHandler[E](showError, httpCode, modifyHttpResponse)
    }

    def fromToString[E]: ErrorHandler.Builder1[E] = ErrorHandler.Builder1[E](_.toString)

    def fromStringEncoder[E: StringEncoder]: ErrorHandler.Builder1[E] = ErrorHandler.Builder1[E](StringEncoder[E].encode)

    def fromJsonEncoder[E: JsonEncoder]: ErrorHandler.Builder1[E] = ErrorHandler.Builder1[E](_.toJson)
    def fromJsonEncoderPretty[E: JsonEncoder]: ErrorHandler.Builder1[E] = ErrorHandler.Builder1[E](_.toJsonPretty)

  }

  // =====| Builders |=====

  final class *[A] private (implicit val decoder: StringDecoder[A])
  object * {
    def apply[A: StringDecoder]: *[A] = new *[A]
  }

  object **

}
