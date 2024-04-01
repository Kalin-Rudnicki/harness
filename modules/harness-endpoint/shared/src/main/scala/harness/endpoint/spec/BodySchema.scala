package harness.endpoint.spec

import harness.endpoint.error.DecodingFailure
import harness.endpoint.transfer.*
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.schema.Schema
import zio.*

sealed trait BodySchema[B <: BodyType] {
  def in(contentLengths: List[Long], stream: java.io.InputStream): IO[DecodingFailure, Receive[B]]
  def out(send: Send[B]): OutputStream
}
object BodySchema {

  final case class Encoded[O](schema: Schema[O]) extends BodySchema[BodyType.Encoded[O]] {

    override def in(contentLengths: List[Long], stream: java.io.InputStream): IO[DecodingFailure, O] =
      contentLengths match {
        case contentLength :: Nil if contentLength > Int.MaxValue =>
          ZIO.fail(DecodingFailure(DecodingFailure.Source.Body, "body length is too long"))
        case _ :: Nil =>
          ZIO.attempt { stream.readAllBytes() }.map(new String(_)).orDie.flatMap {
            schema.decode(_) match {
              case Right(value) => ZIO.succeed(value)
              case Left(error)  => ZIO.fail(DecodingFailure(DecodingFailure.Source.Body, error))
            }
          }
        case Nil =>
          ZIO.fail(DecodingFailure(DecodingFailure.Source.Body, "expected input body (missing Content-Length header)"))
        case _ :: _ :: _ =>
          // TODO (KR) :
          ZIO.fail(DecodingFailure(DecodingFailure.Source.Body, "received multiple Content-Length headers"))
      }

    override def out(send: O): OutputStream =
      OutputStream.Str(schema.encode(send))

  }

  case object Stream extends BodySchema[BodyType.Stream] {

    override def in(contentLengths: List[Long], stream: java.io.InputStream): IO[DecodingFailure, InputStream] =
      contentLengths match {
        case _ :: Nil =>
          ZIO.succeed(InputStream(stream))
        case Nil =>
          ZIO.fail(DecodingFailure(DecodingFailure.Source.Body, "expected input body (missing Content-Length header)"))
        case _ :: _ :: _ =>
          // TODO (KR) :
          ZIO.fail(DecodingFailure(DecodingFailure.Source.Body, "received multiple Content-Length headers"))
      }

    override def out(send: OutputStream): OutputStream =
      send

  }

  case object None extends BodySchema[BodyType.None] {

    override def in(contentLengths: List[Long], stream: java.io.InputStream): IO[DecodingFailure, Unit] =
      ZIO.unit

    override def out(send: Send[BodyType.None]): OutputStream =
      OutputStream.Empty

  }

}
