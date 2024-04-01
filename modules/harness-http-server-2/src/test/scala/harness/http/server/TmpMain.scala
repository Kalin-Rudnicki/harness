package harness.http.server

import cats.syntax.option.*
import harness.endpoint.spec.*
import harness.endpoint.typeclass.*
import harness.endpoint.types.*
import harness.web.HttpCode
import harness.zio.*
import java.util.UUID
import zio.*
import zio.json.*

object TmpMain extends ExecutableApp {

  @jsonDiscriminator("type")
  sealed trait ApiError
  object ApiError {

    @errorCode(HttpCode.`400`)
    @errorExamples(BadRequest("Invalid Input"))
    final case class BadRequest(message: String) extends ApiError

    @errorCode(HttpCode.`500`)
    @errorExamples(InternalDefect)
    case object InternalDefect extends ApiError

    implicit val errorSchema: ErrorSchema.ForJson[ApiError] = ErrorSchema.derive

  }

  sealed trait DomainError {

    def toApi: ApiError = this match {
      case DomainError.DecodingFailure(message) => ApiError.BadRequest(message)
      case DomainError.InternalDefect(_)        => ApiError.InternalDefect
    }

  }
  object DomainError {

    final case class DecodingFailure(message: String) extends DomainError

    final case class InternalDefect(error: Throwable) extends DomainError

  }

  implicit val errorHandler: ErrorHandler[DomainError, ApiError] =
    ErrorHandler[DomainError, ApiError](
      convertDecodingFailure = e => DomainError.DecodingFailure(e.getMessage),
      convertUnexpectedError = DomainError.InternalDefect(_),
      errorConverter = _.toApi,
      errorLogger = ErrorLogger.withToString[DomainError].atLevel.error,
      headersAndCookiesOnError = _ => identity,
    )

  trait Service1 {
    def sayHi(message: String): URIO[Logger, Unit]
  }
  object Service1 {

    val layer: ULayer[Service1] =
      ZLayer.succeed {
        new Service1 {
          override def sayHi(message: String): URIO[Logger, Unit] = Logger.log.info(message)
        }
      }

  }

  final case class Pattern1[F[_ <: EndpointType.Any]](
      route1: F[Pattern1.Route1],
      route2: F[Pattern1.Route2],
  )
  object Pattern1 {

    type Route1 = EndpointType[UUID, UUID, BodyType.None, BodyType.Encoded[String], ApiError]
    type Route2 = EndpointType[(UUID, String), (UUID, String), BodyType.None, BodyType.Encoded[String], ApiError]

  }

  val specs: Pattern1[EndpointSpec] =
    Pattern1[EndpointSpec](
      route1 = EndpointSpec.get("Route 1") / "route-1" / path[UUID]("id") /--> body.raw[String] /!--> errorBody.json[ApiError],
      route2 = EndpointSpec.get("Route 2") / "route-2" /? query[UUID]("id") /# header.raw[String]("Auth") /--> body.raw[String] /!--> errorBody.json[ApiError],
    )

  val impls: Pattern1[Implementation.Projection[Service1]] =
    Pattern1[Implementation.Projection[Service1]](
      route1 = Implementation[Pattern1.Route1] { (id: UUID) =>
        ZIO
          .serviceWithZIO[Service1](_.sayHi(id.toString))
          .as(HttpResponse(id.toString))
      },
      route2 = Implementation[Pattern1.Route2] { (id: UUID, auth: String) =>
        Logger.log.info(s"auth: $auth") *>
          ZIO
            .serviceWithZIO[Service1](_.sayHi(id.toString))
            .as(HttpResponse(id.toString))
      },
    )

  val endpoint: Pattern1[Endpoint.Projection[Service1]] =
    Endpoint.make("api" /: specs, impls)

  override val executable: Executable =
    Executable
      .withLayer {
        ZLayer.succeed {
          Server.Config(
            3030.some,
            "res",
            true,
            None,
            true,
          )
        }
      }
      .withThrowableEffect {
        for {
          _ <- Logger.log.info("=====| Tmp Main |=====")
          _ <- Server.start[Any, Service1, Pattern1](
            Service1.layer,
            endpoint,
          )
        } yield ()
      }

}
