package harness.http.server

import harness.core.*
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.zio.*
import zio.*

trait Implementation[-R, ET <: EndpointType.Any] {

  type DomainError

  val errorHandler: ErrorHandler[DomainError, Error[ET]]

  val impl: (InputWithCookies[ET], Receive[InputBody[ET]]) => ZIO[HarnessEnv & Implementation.Provided & R, DomainError, HttpResponse[Send[OutputBody[ET]]]]

}
object Implementation {

  type Projection[R] = [ET <: EndpointType.Any] =>> Implementation[R, ET]
  type Provided = HttpRequest

  def apply[ET <: EndpointType.Any](using zip: Zip[InputWithCookies[ET], Receive[InputBody[ET]]]): Builder[zip.Out, ET] = new Builder[zip.Out, ET](zip)

  final class Builder[I, ET <: EndpointType.Any](zip: Zip.Out[InputWithCookies[ET], Receive[InputBody[ET]], I]) {

    def apply[_R, _DomainError](
        f: I => ZIO[HarnessEnv & Implementation.Provided & _R, _DomainError, HttpResponse[Send[OutputBody[ET]]]],
    )(using _errorHandler: ErrorHandler[_DomainError, Error[ET]]): Implementation[_R, ET] =
      new Implementation[_R, ET] {
        override type DomainError = _DomainError
        override val errorHandler: ErrorHandler[_DomainError, Error[ET]] = _errorHandler
        override val impl: (InputWithCookies[ET], Receive[InputBody[ET]]) => ZIO[HarnessEnv & Implementation.Provided & _R, _DomainError, HttpResponse[Send[OutputBody[ET]]]] =
          (i1, i2) => f(zip.zip(i1, i2))
      }

  }

}
