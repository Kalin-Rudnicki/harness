package harness.endpoint.spec

import harness.endpoint.error.DecodingFailure
import harness.endpoint.transfer.*
import harness.endpoint.types.*
import harness.endpoint.types.Types.*
import harness.schema.Schema
import zio.*

sealed trait BodyCodec[B <: BodyType] {
  def in(contentLengths: List[Long], stream: java.io.InputStream): IO[DecodingFailure, Receive[B]]
  def out(send: Send[B]): OutputStream
}
object BodyCodec {

  final case class Encoded[O](schema: Schema[O]) extends BodyCodec[BodyType.Encoded[O]] {

    override def in(contentLengths: List[Long], stream: java.io.InputStream): IO[DecodingFailure, O] =
      contentLengths match {
        case contentLength :: Nil if contentLength > Int.MaxValue =>
          ZIO.fail(DecodingFailure(SchemaSource.Body :: Nil, DecodingFailure.Cause.ContentLengthTooLong))
        case _ :: Nil =>
          ZIO.attempt { stream.readAllBytes() }.map(new String(_)).orDie.flatMap {
            schema.decode(_) match {
              case Right(value) => ZIO.succeed(value)
              case Left(error)  => ZIO.fail(DecodingFailure(SchemaSource.Body :: Nil, DecodingFailure.Cause.DecodeFail(error)))
            }
          }
        case Nil =>
          ZIO.fail(DecodingFailure(SchemaSource.Body :: Nil, DecodingFailure.Cause.MissingContentLength))
        case _ :: _ :: _ =>
          // TODO (KR) :
          ZIO.fail(DecodingFailure(SchemaSource.Body :: Nil, DecodingFailure.Cause.MultipleContentLengths))
      }

    override def out(send: O): OutputStream =
      OutputStream.Str(schema.encode(send))

  }

  case object Stream extends BodyCodec[BodyType.Stream] {

    override def in(contentLengths: List[Long], stream: java.io.InputStream): IO[DecodingFailure, InputStream] =
      contentLengths match {
        case _ :: Nil =>
          ZIO.succeed(InputStream(stream))
        case Nil =>
          ZIO.fail(DecodingFailure(SchemaSource.Body :: Nil, DecodingFailure.Cause.MissingContentLength))
        case _ :: _ :: _ =>
          // TODO (KR) :
          ZIO.fail(DecodingFailure(SchemaSource.Body :: Nil, DecodingFailure.Cause.MultipleContentLengths))
      }

    override def out(send: OutputStream): OutputStream =
      send

  }

  case object None extends BodyCodec[BodyType.None] {

    override def in(contentLengths: List[Long], stream: java.io.InputStream): IO[DecodingFailure, Unit] =
      ZIO.unit

    override def out(send: Send[BodyType.None]): OutputStream =
      OutputStream.Empty

  }

}
