package harness.http.server

import cats.syntax.option.*
import harness.endpoint.spec.*
import harness.endpoint.typeclass.*
import harness.endpoint.types.*
import harness.http.client.*
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

    implicit val errorSchema: ErrorSchema[ApiError] = ErrorSchema.derive

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

  implicit val errorHandler: ErrorHandler.Id[DomainError, ApiError] =
    ErrorHandler.id[DomainError, ApiError](
      convertDecodingFailure = e => DomainError.DecodingFailure(e.getMessage),
      convertUnexpectedError = DomainError.InternalDefect(_),
      convertDomainError = _.toApi,
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
      route3: F[Pattern1.Route3],
  )
  object Pattern1 {

    type Route1 = EndpointType.Basic[UUID, BodyType.None, BodyType.Encoded[String], ApiError]
    type Route2 = EndpointType.Basic[(UUID, String), BodyType.None, BodyType.Encoded[String], ApiError]
    type Route3 = EndpointType.Basic[UUID, BodyType.None, BodyType.Encoded[String], ApiError]

  }

  val specs: Pattern1[EndpointSpec] =
    "api" /: Pattern1[EndpointSpec](
      route1 = EndpointSpec.get("Route 1") / "route-1" / path[UUID]("id") /--> body.raw[String] /!--> errorBody.json[ApiError],
      route2 = EndpointSpec.get("Route 2") / "route-2" /? query[UUID]("id") /# header.raw[String]("Auth") /--> body.raw[String] /!--> errorBody.json[ApiError],
      route3 = EndpointSpec.get("Route 3") / "route-3" / path[UUID]("id") /--> body.raw[String] /!--> errorBody.json[ApiError],
    )

  val impls: Pattern1[Implementation.Projection[Service1]] =
    Pattern1[Implementation.Projection[Service1]](
      route1 = Implementation[Pattern1.Route1].implement { (id: UUID) =>
        ZIO
          .serviceWithZIO[Service1](_.sayHi(id.toString))
          .as(HttpResponse(id.toString))
      },
      route2 = Implementation[Pattern1.Route2].implement { (id: UUID, auth: String) =>
        Logger.log.info(s"auth: $auth") *>
          ZIO
            .serviceWithZIO[Service1](_.sayHi(id.toString))
            .as(HttpResponse(id.toString))
      },
      route3 = Implementation[Pattern1.Route3].implement { (id: UUID) =>
        ZIO.fail(DomainError.InternalDefect(new RuntimeException("Oh no!!!")): DomainError)
      },
    )

  val endpoint: Pattern1[Endpoint.Projection[Service1]] =
    Endpoint.make(specs, impls)

  val client: Pattern1[EndpointSend] =
    EndpointSend.make("http://localhost:3030", specs)

  override val executable: Executable =
    Executable.fromSubCommands(
      "server" -> Executable
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
            _ <- Logger.log.info("=====| Server |=====")
            _ <- Server.start[Any, Service1 & String, Pattern1, Service1](
              Service1.layer ++ ZLayer.succeed(""),
              endpoint,
            )
          } yield ()
        },
      "client" -> Executable.withLayer { HttpClient.defaultLayer }.withEffect {
        for {
          _ <- Logger.log.info("=====| Client |=====")
          res1 <- client.route1(UUID.randomUUID).either
          res2 <- client.route2((UUID.randomUUID, "my-auth")).either
          res3 <- client.route3(UUID.randomUUID).either
          _ <- Logger.log.info(res1)
          _ <- Logger.log.info(res2)
          _ <- Logger.log.info(res3)
        } yield ()
      },
    )

}
