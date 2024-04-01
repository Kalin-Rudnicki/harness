package harness.endpoint.types

sealed trait BodyType
object BodyType {
  sealed trait Basic extends BodyType

  final class None private () extends BodyType.Basic
  final class Stream private () extends BodyType.Basic
  final class Encoded[O] private () extends BodyType
}
