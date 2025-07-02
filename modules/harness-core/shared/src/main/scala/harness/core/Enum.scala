package harness.core

import cats.data.NonEmptyList

trait Enum[E <: Enum[E]] extends java.lang.Enum[E] { self: E => }
object Enum {

  inline def values[E <: Enum[E]](implicit hc: HasCompanion[E]): Seq[E] = hc.companion.enumValues

  // =====|  |=====

  final case class HasCompanion[E <: Enum[E]] private[Enum] (companion: Companion[E])

  trait Companion[E <: Enum[E]] {

    implicit final val hasCompanion: HasCompanion[E] = Enum.HasCompanion(this)

    protected val defaultToString: E => NonEmptyList[String] = e => NonEmptyList.one(e.toString)

    def values: Array[E]

    final lazy val enumValues: Seq[E] = values.toSeq

    abstract class EnumMap[Enc](enc: E => NonEmptyList[Enc]) {
      final lazy val encodedValues: Seq[Enc] = enumValues.flatMap(enc(_).toList)

      protected lazy val map: Map[Enc, E] = values.flatMap { e => enc(e).toList.map((_, e)) }.toMap
      def encode(e: E): Enc = enc(e).head
      def decode(enc: Enc): Option[E] = map.get(enc)
    }

    abstract class CaseInsensitiveStringMap(enc: E => NonEmptyList[String]) extends EnumMap[String](enc) {
      override protected lazy val map: Map[String, E] = values.flatMap { e => enc(e).toList.map(str => (str.toUpperCase, e)) }.toMap
      override def decode(enc: String): Option[E] = map.get(enc.toUpperCase)
    }

    implicit object ToString extends CaseInsensitiveStringMap(defaultToString)

    implicit val stringEncoder: StringEncoder[E] = ToString.encode(_)
    implicit val stringDecoder: StringDecoder[E] = StringDecoder.fromOptionF("LogLevel", ToString.decode)

  }

  // =====|  |=====

  trait WithEnc[E <: Enum[E], Enc] private[Enum] {
    def values: Seq[E]
    def encodedValues: Seq[Enc]
    def encode(e: E): Enc
    def decode(enc: Enc): Option[E]
  }
  object WithEnc {

    inline def apply[E <: Enum[E], Enc](implicit ewe: Enum.WithEnc[E, Enc]): WithEnc[E, Enc] = ewe

    given [E <: Enum[E], Enc] => (hc: HasCompanion[E], em: Enum.Companion[E]#EnumMap[Enc]) => WithEnc[E, Enc] =
      new WithEnc[E, Enc] {
        override def values: Seq[E] = hc.companion.enumValues
        override def encodedValues: Seq[Enc] = em.encodedValues
        override def encode(e: E): Enc = em.encode(e)
        override def decode(enc: Enc): Option[E] = em.decode(enc)
      }

  }

}
