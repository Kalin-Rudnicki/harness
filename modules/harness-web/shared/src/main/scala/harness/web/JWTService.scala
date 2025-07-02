package harness.web

import harness.web.error.JWTError
import zio.*
import zio.json.*

trait JWTService {

  def makeJWT[A: {JsonEncoder, JWTPayload}](payload: A): UIO[JWT[A]]
  def validateJWT[A: JWTPayload](jwt: JWT[A]): IO[JWTError, Unit]

  final def parseAndValidateJWT[A: {JsonDecoder, JWTPayload}](raw: RawJWT): IO[JWTError, A] =
    for {
      jwt <- ZIO.fromEither(JWT.fromRaw[A](raw)).mapError(JWTError.UnableToDecodePayload(_))
      _ <- validateJWT(jwt)
    } yield jwt.payload

}
object JWTService {

  final case class Live(
      config: Live.Config,
  ) extends JWTService {

    override def makeJWT[A: {JsonEncoder, JWTPayload}](payload: A): UIO[JWT[A]] =
      Clock.instant.map { now =>
        JWT.make(config.alg, config.typ, config.key)(JWTPayload[A].updateExpiration(payload, now.plus(config.duration)))
      }

    override def validateJWT[A: JWTPayload](jwt: JWT[A]): IO[JWTError, Unit] =
      for {
        now <- Clock.instant
        _ <- ZIO.fail(JWTError.InvalidSignature).unless(jwt.raw.verifySignature(config.key))
        _ <- ZIO.fail(JWTError.ExpiredToken).when(now.isAfter(JWTPayload[A].getExpiration(jwt.payload)))
      } yield ()

  }
  object Live {

    final case class Config(
        alg: JWTHeader.Alg,
        typ: JWTHeader.Type,
        duration: Duration,
        key: String,
    ) derives JsonCodec

    def layer: URLayer[Live.Config, JWTService] =
      ZLayer.fromFunction { Live.apply }

    def make(duration: Duration, key: String): JWTService.Live =
      Live(Config(JWTHeader.Alg.HS256, JWTHeader.Type.JWT, duration, key))

  }

}
