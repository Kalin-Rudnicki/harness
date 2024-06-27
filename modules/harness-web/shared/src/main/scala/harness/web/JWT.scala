package harness.web

import cats.data.NonEmptyList
import cats.syntax.either.*
import harness.core.*
import java.time.Instant
import java.util.Base64
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import scala.util.Try
import zio.json.*

final case class JWTHeader(alg: JWTHeader.Alg, typ: JWTHeader.Type) derives JsonCodec
object JWTHeader {

  enum Alg extends Enum[Alg] { case HS256 }
  object Alg extends Enum.Companion[Alg]

  enum Type extends Enum[Type] { case JWT }
  object Type extends Enum.Companion[Type]

}

trait JWTPayload[A <: JWTPayload[A]] { self: A =>
  val exp: Instant
  def updateExp(exp: Instant): A
}

final case class RawJWT private (
    headerBase64: String,
    header: JWTHeader,
    payloadBase64: String,
    payload: String,
    signatureBase64: String,
) {

  def verifySignature(key: String): Boolean =
    RawJWT.makeSignature(header.alg, key)(headerBase64, payloadBase64) == signatureBase64

  def bearer: String = s"Bearer $headerBase64.$payloadBase64.$signatureBase64"

}
object RawJWT {

  private val reg = "^Bearer ([^.]+)\\.([^.]+)\\.([^.]+)$".r
  private val enc = Base64.getUrlEncoder.withoutPadding
  private val dec = Base64.getUrlDecoder

  private def base64Decode(string: String): Either[String, String] =
    Try { new String(dec.decode(string)) }.toEither.leftMap(_.safeGetMessage)

  private def makeSignature(alg: JWTHeader.Alg, key: String)(headerBase64: String, payloadBase64: String): String =
    alg match {
      case JWTHeader.Alg.HS256 =>
        val mac = Mac.getInstance("HMacSHA256")
        mac.init(new SecretKeySpec(key.getBytes, "HMacSHA256"))
        enc.encodeToString(mac.doFinal(s"$headerBase64.$payloadBase64".getBytes))
    }

  def make(alg: JWTHeader.Alg, `type`: JWTHeader.Type, key: String)(payload: String): RawJWT = {
    val header = JWTHeader(alg, `type`)
    val headerBase64 = enc.encodeToString(header.toJson.getBytes)
    val payloadBase64 = enc.encodeToString(payload.getBytes)

    RawJWT(
      headerBase64 = headerBase64,
      header = header,
      payloadBase64 = payloadBase64,
      payload = payload,
      signatureBase64 = makeSignature(alg, key)(headerBase64, payloadBase64),
    )
  }

  implicit val stringEncoder: StringEncoder[RawJWT] =
    _.toString

  implicit val stringDecoder: StringDecoder[RawJWT] = {
    case reg(headerBase64, payloadBase64, signatureBase64) =>
      (for {
        un64edHeader <- base64Decode(headerBase64)
        header <- un64edHeader.fromJson[JWTHeader]
        un64edPayload <- base64Decode(payloadBase64)
      } yield RawJWT(
        headerBase64 = headerBase64,
        header = header,
        payloadBase64 = payloadBase64,
        payload = un64edPayload,
        signatureBase64 = signatureBase64,
      )).leftMap(NonEmptyList.one)
    case _ =>
      "Malformed bearer token".leftNel
  }

}

final case class JWT[A] private (
    raw: RawJWT,
    payload: A,
) {
  def bearer: String = raw.bearer
}
object JWT {

  def make[A: JsonEncoder](alg: JWTHeader.Alg, `type`: JWTHeader.Type, key: String)(payload: A): JWT[A] =
    JWT(
      RawJWT.make(alg, `type`, key)(payload.toJson),
      payload,
    )

  def fromRaw[A: JsonDecoder](raw: RawJWT): Either[String, JWT[A]] =
    raw.payload.fromJson[A].map(JWT(raw, _))

}
