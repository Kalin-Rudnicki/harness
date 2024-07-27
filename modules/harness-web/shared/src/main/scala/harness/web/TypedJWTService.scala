package harness.web

import harness.web.error.JWTError
import zio.*
import zio.json.*

final class TypedJWTService[A](
    jwtService: JWTService,
)(implicit
    encoder: JsonEncoder[A],
    decoder: JsonDecoder[A],
    jwtPayload: JWTPayload[A],
) {
  def makeJWT(payload: A): UIO[JWT[A]] = jwtService.makeJWT[A](payload)
  def validateJWT(jwt: JWT[A]): IO[JWTError, Unit] = jwtService.validateJWT[A](jwt)
  def parseAndValidateJWT(raw: RawJWT): IO[JWTError, A] = jwtService.parseAndValidateJWT[A](raw)
}
object TypedJWTService {

  def layer[A: JsonEncoder: JsonDecoder: JWTPayload: Tag]: URLayer[JWTService, TypedJWTService[A]] =
    ZLayer.service[JWTService].project(new TypedJWTService[A](_))

}
