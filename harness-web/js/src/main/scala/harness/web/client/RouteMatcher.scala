package harness.web.client

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.core.*
import scala.annotation.{tailrec, unused}

trait RouteMatcher[I] private { self =>

  // =====| Public API |=====

  final def apply(
      path: List[String],
      params: Map[String, String],
  )(implicit ev: Unit <:< I): RouteMatcher.Result =
    self.routeInternal(ev(()), path, params, Nil, Nil)

  final def apply(
      url: Url,
  )(implicit ev: Unit <:< I): RouteMatcher.Result =
    self.routeInternal(ev(()), url.path, url.params, Nil, Nil)

  /**
    * Will consume a string from the path
    */
  final def /:(str: String): RouteMatcher[I] =
    (i, path, params, rPPath, rPParams) =>
      path match {
        case value :: tail if value == str => self.routeInternal(i, tail, params, value :: rPPath, rPParams)
        case _                             => RouteMatcher.Result.NotFound
      }

  /**
    * Will consume all strings from the path
    */
  final def /:[O](@unused ra: RouteMatcher.RemainingArgs.type)(implicit zip: Zip.Out[O, List[String], I]): RouteMatcher[O] =
    (i, path, params, rPPath, rPParams) => self.routeInternal(zip.zip(i, path), Nil, params, path reverse_::: rPPath, rPParams)

  /**
    * Will consume a string from the path, and that string must be decoded
    */
  final def /:[A, O](arg: RouteMatcher.PathArg[A])(implicit zip: Zip.Out[O, A, I]): RouteMatcher[O] =
    (i, path, params, rPPath, rPParams) =>
      path match {
        case value :: tail =>
          arg.decoder.decodeAccumulating(value) match {
            case Right(decoded) => self.routeInternal(zip.zip(i, decoded), tail, params, value :: rPPath, rPParams)
            case Left(errors)   => RouteMatcher.Result.Fail(errors.map(HError.UserError(_)))
          }
        case Nil => RouteMatcher.Result.NotFound
      }

  /**
    * Will attempt to parse a string from query-params.<br>
    * Will return Fail if not present.<br>
    * Will return Fail if unable to parse.
    */
  final def ?:[A, O](arg: RouteMatcher.ParamArg[A])(implicit zip: Zip.Out[O, A, I]): RouteMatcher[O] =
    (i, path, params, rPPath, rPParams) =>
      params.get(arg.param) match {
        case Some(value) =>
          arg.decoder.decodeAccumulating(value) match {
            case Right(decoded) => self.routeInternal(zip.zip(i, decoded), path, params, rPPath, (arg.param, value) :: rPParams)
            case Left(errors)   => RouteMatcher.Result.Fail(errors.map(HError.UserError(_)))
          }
        case None =>
          RouteMatcher.Result.Fail(NonEmptyList.one(HError.UserError(s"Missing query param: '${arg.param}'")))
      }

  /**
    * Will attempt to parse a string from query-params.<br>
    * Will yield None if not present.<br>
    * Will return Fail if unable to parse.
    */
  final def ??:[A, O](arg: RouteMatcher.ParamArg[A])(implicit zip: Zip.Out[O, Option[A], I]): RouteMatcher[O] =
    (i, path, params, rPPath, rPParams) =>
      params.get(arg.param) match {
        case Some(value) =>
          arg.decoder.decodeAccumulating(value) match {
            case Right(decoded) => self.routeInternal(zip.zip(i, decoded.some), path, params, rPPath, (arg.param, value) :: rPParams)
            case Left(errors)   => RouteMatcher.Result.Fail(errors.map(HError.UserError(_)))
          }
        case None =>
          self.routeInternal(zip.zip(i, None), path, params, rPPath, rPParams)
      }

  /**
    * Will attempt to parse a string from query-params.<br>
    * Will return NotFound if not present.<br>
    * Will return NotFound if unable to parse.
    */
  final def ?*:[A, O](arg: RouteMatcher.ParamArg[A])(implicit zip: Zip.Out[O, A, I]): RouteMatcher[O] =
    (i, path, params, rPPath, rPParams) =>
      params.get(arg.param) match {
        case Some(value) =>
          arg.decoder.decodeAccumulating(value) match {
            case Right(decoded) => self.routeInternal(zip.zip(i, decoded), path, params, rPPath, (arg.param, value) :: rPParams)
            case Left(_)        => RouteMatcher.Result.NotFound
          }
        case None =>
          RouteMatcher.Result.NotFound
      }

  /**
    * Will attempt to parse a string from query-params.<br>
    * Will yield None if not present.<br>
    * Will return NotFound if unable to parse.
    */
  final def ??*:[A, O](arg: RouteMatcher.ParamArg[A])(implicit zip: Zip.Out[O, Option[A], I]): RouteMatcher[O] =
    (i, path, params, rPPath, rPParams) =>
      params.get(arg.param) match {
        case Some(value) =>
          arg.decoder.decodeAccumulating(value) match {
            case Right(decoded) => self.routeInternal(zip.zip(i, decoded.some), path, params, rPPath, (arg.param, value) :: rPParams)
            case Left(_)        => RouteMatcher.Result.NotFound
          }
        case None =>
          self.routeInternal(zip.zip(i, None), path, params, rPPath, rPParams)
      }

  final def imap[I2](f: I2 => I): RouteMatcher[I2] =
    (i, path, params, rPPath, rPParams) => self.routeInternal(f(i), path, params, rPPath, rPParams)

  // =====| Abstract |=====

  def routeInternal(
      i: I,
      path: List[String],
      params: Map[String, String],
      rParsedPath: List[String],
      rParsedParams: List[(String, String)],
  ): RouteMatcher.Result

}
object RouteMatcher {

  enum Result {
    case Success(page: Page)
    case Fail(errors: NonEmptyList[HError])
    case NotFound
  }

  type Root = RouteMatcher[Unit]

  // =====| Builders |=====

  final class PathArg[A] private (implicit val decoder: StringDecoder[A])
  object PathArg {
    def apply[A: StringDecoder]: PathArg[A] = new PathArg[A]
  }

  final class ParamArg[A] private (val param: String)(implicit val decoder: StringDecoder[A])
  object ParamArg {
    def apply[A: StringDecoder](param: String): ParamArg[A] = new ParamArg[A](param)
  }

  object RemainingArgs

  // =====| Implementations |=====

  def finish[I](f: I => Url => Page): RouteMatcher[I] =
    (i, path, _, rPPath, rPParams) =>
      if (path.isEmpty) Result.Success(f(i)(Url(rPPath.reverse, rPParams.reverse.toMap)))
      else Result.NotFound

  inline def const(page: Url => Page): RouteMatcher.Root =
    RouteMatcher.finish[Unit] { _ => page(_) }

  inline def constIgnorePath(page: Url => Page): RouteMatcher.Root =
    RemainingArgs /: finish[List[String]] { _ => page }

  def oneOf[I](children: RouteMatcher[I]*): RouteMatcher[I] = { (i, path, params, rPPath, rPParams) =>
    @tailrec
    def loop(children: List[RouteMatcher[I]]): RouteMatcher.Result =
      children match {
        case head :: tail =>
          head.routeInternal(i, path, params, rPPath, rPParams) match {
            case Result.NotFound => loop(tail)
            case result          => result
          }
        case Nil => RouteMatcher.Result.NotFound
      }

    loop(children.toList)
  }

  inline def root(children: RouteMatcher.Root*): RouteMatcher.Root = RouteMatcher.oneOf[Unit](children*)

}
