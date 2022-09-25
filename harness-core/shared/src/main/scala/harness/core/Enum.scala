package harness.core

trait Enum[E <: Enum[E]] extends java.lang.Enum[E] { self: E => }
object Enum {

  inline def values[E <: Enum[E]](implicit hc: HasCompanion[E]): Set[E] = hc.companion.enumValues

  // =====|  |=====

  final case class HasCompanion[E <: Enum[E]] private[Enum] (companion: Companion[E])

  trait Companion[E <: Enum[E]] {

    implicit final val hasCompanion: HasCompanion[E] = Enum.HasCompanion(this)

    def values: Array[E]

    final lazy val enumValues: Set[E] = values.toSet

    abstract class EnumMap[Enc](enc: E => Enc) {
      private final lazy val map: Map[Enc, E] = values.map { e => (enc(e), e) }.toMap
      final def encode(e: E): Enc = enc(e)
      final def decode(enc: Enc): Option[E] = map.get(enc)
    }

    implicit object ToString extends EnumMap[String](_.toString)

  }

  // =====|  |=====

  trait WithEnc[E <: Enum[E], Enc] private[Enum] {
    def values: Set[E]
    def encode(e: E): Enc
    def decode(enc: Enc): Option[E]
  }
  object WithEnc {

    inline def apply[E <: Enum[E], Enc](implicit ewe: Enum.WithEnc[E, Enc]): WithEnc[E, Enc] = ewe

    given [E <: Enum[E], Enc](using hc: HasCompanion[E], em: Enum.Companion[E]#EnumMap[Enc]): WithEnc[E, Enc] =
      new WithEnc[E, Enc] {
        override def values: Set[E] = hc.companion.enumValues
        override def encode(e: E): Enc = em.encode(e)
        override def decode(enc: Enc): Option[E] = em.decode(enc)
      }

  }

}
