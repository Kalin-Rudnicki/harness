package harness.pk

import java.time.Instant
import java.util.UUID
import zio.*

opaque type TUID = UUID
object TUID {

  extension (self: TUID) {
    def toUUID: UUID = self
  }

  /**
    * This should only be called on something that is known to have been generated using [[TUID.genNow]]
    */
  def unsafeWrap(uuid: UUID): TUID = uuid

  private val numTimeBits = 36
  private val numNonTimeHighBits = 128 - numTimeBits
  private val highBytesAlias =
    if (numTimeBits == 0) -1L
    else Long.MaxValue >> (numTimeBits - 1)

  def make(instant: Instant, highBytes: Long, lowBytes: Long): TUID =
    new UUID(
      (instant.getEpochSecond << numNonTimeHighBits) | (highBytes & highBytesAlias),
      lowBytes,
    )

  val genNow: UIO[TUID] =
    for {
      now <- Clock.instant
      highBytes <- Random.nextLong
      lowBytes <- Random.nextLong
    } yield TUID.make(now, highBytes, lowBytes)

}
