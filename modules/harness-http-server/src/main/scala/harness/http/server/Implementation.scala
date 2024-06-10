package harness.http.server

import harness.core.*
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.zio.{Path as _, *}
import zio.*

trait Implementation[-R, ET <: EndpointType.Any] {

  type DomainError

  val errorHandler: ErrorHandler[DomainError, Error[ET]]

  val impl: (
      Path[ET],
      Query[ET],
      Auth[ET],
      Header[ET],
      Receive[InputBody[ET]],
  ) => ZIO[HarnessEnv & Implementation.Provided & R, DomainError, HttpResponse[Send[OutputBody[ET]]]]

}
object Implementation {

  type Projection[R] = [ET <: EndpointType.Any] =>> Implementation[R, ET]
  type Provided = Scope & HttpRequest

  def apply[ET <: EndpointType.Any](using zip: Zip5[Path[ET], Query[ET], Auth[ET], Header[ET], Receive[InputBody[ET]]]): Builder[zip.Out, ET] = new Builder[zip.Out, ET](zip)

  final class Builder[I, ET <: EndpointType.Any](zip: Zip5.Out[Path[ET], Query[ET], Auth[ET], Header[ET], Receive[InputBody[ET]], I]) {

    def implement[_R, _DomainError](
        f: I => ZIO[HarnessEnv & Implementation.Provided & _R, _DomainError, HttpResponse[Send[OutputBody[ET]]]],
    )(using _errorHandler: ErrorHandler.Aux[_DomainError, ?, Error[ET]]): Implementation[_R, ET] =
      new Implementation[_R, ET] {
        override type DomainError = _errorHandler._DomainError
        override val errorHandler: ErrorHandler[_errorHandler._DomainError, Error[ET]] = _errorHandler
        override val impl: (
            Path[ET],
            Query[ET],
            Auth[ET],
            Header[ET],
            Receive[InputBody[ET]],
        ) => ZIO[HarnessEnv & Implementation.Provided & _R, _errorHandler._DomainError, HttpResponse[Send[OutputBody[ET]]]] =
          (p, q, a, h, b) => f(zip.zip(p, q, a, h, b)).mapError(_errorHandler.convertErr.mapError)
      }

  }

}
