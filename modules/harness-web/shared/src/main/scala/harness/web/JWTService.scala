package harness.web

import harness.web.error.JWTError
import zio.*
import zio.json.*

trait JWTService[A] {

  def makeJWT(payload: A): UIO[JWT[A]]
  def validateJWT(raw: RawJWT): IO[JWTError, JWT[A]]

  private def parseBearer(bearer: String): IO[JWTError, RawJWT] =
    ZIO.fromEither(RawJWT.stringDecoder.decode(bearer)).mapError(JWTError.UnableToDecodeBody(_))

  final def validateJWT(bearer: String): IO[JWTError, JWT[A]] = parseBearer(bearer).flatMap(validateJWT(_))
  final def getPayload(raw: RawJWT): IO[JWTError, A] = validateJWT(raw).map(_.payload)
  final def getPayload(bearer: String): IO[JWTError, A] = parseBearer(bearer).flatMap(getPayload(_))

}
object JWTService {

  final case class Live[A <: JWTPayload[A]](
      config: Live.Config,
      jsonCodec: JsonCodec[A],
  ) extends JWTService[A] {

    private given JsonEncoder[A] = jsonCodec.encoder
    private given JsonDecoder[A] = jsonCodec.decoder

    override def makeJWT(payload: A): UIO[JWT[A]] =
      Clock.instant.map { now =>
        JWT.make(config.alg, config.typ, config.key)(payload.updateExp(now.plus(config.duration)))
      }

    override def validateJWT(raw: RawJWT): IO[JWTError, JWT[A]] =
      for {
        now <- Clock.instant
        _ <- ZIO.fail(JWTError.InvalidSignature).unless(raw.verifySignature(config.key))
        jwt <- ZIO.fromEither(JWT.fromRaw[A](raw)).mapError(JWTError.UnableToDecodeBody(_))
        _ <- ZIO.fail(JWTError.ExpiredToken).when(now.isAfter(jwt.payload.exp))
      } yield jwt

  }
  object Live {

    def layer[A <: JWTPayload[A]: JsonCodec: Tag]: URLayer[Live.Config, JWTService[A]] =
      ZLayer.fromZIO { ZIO.serviceWith[Live.Config](Live(_, JsonCodec[A])) }

    def make[A <: JWTPayload[A]: JsonCodec](duration: Duration, key: String): JWTService.Live[A] =
      Live(Config(JWTHeader.Alg.HS256, JWTHeader.Type.JWT, duration, key), JsonCodec[A])

    final case class Config(
        alg: JWTHeader.Alg,
        typ: JWTHeader.Type,
        duration: Duration,
        key: String,
    ) derives JsonCodec

  }

}
