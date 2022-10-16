package harness.web.server

import harness.core.*
import harness.web.*

given Conversion[String, RouteMatcher[Unit]] = { str => (_, path) =>
  path match {
    case head :: tail if head == str => RouteMatcher.Result.Success(tail, ())
    case _                           => RouteMatcher.Result.NotFound
  }
}
given Conversion[HttpMethod, RouteMatcher[Unit]] = { m1 => (m2, path) =>
  if (m1 == m2) RouteMatcher.Result.Success(path, ())
  else RouteMatcher.Result.NotFound
}
given Conversion[RouteMatcher.**.type, RouteMatcher[List[String]]] = { _ => (_, path) =>
  RouteMatcher.Result.Success(Nil, path)
}
given [A]: Conversion[RouteMatcher.*[A], RouteMatcher[A]] = { arg => (_, path) =>
  path match {
    case head :: tail =>
      arg.decoder.decodeAccumulating(head) match {
        case Right(value) => RouteMatcher.Result.Success(tail, value)
        case Left(errors) => RouteMatcher.Result.Fail(HError(errors.map(HError.UserError(_))))
      }
    case _ => RouteMatcher.Result.NotFound
  }
}
