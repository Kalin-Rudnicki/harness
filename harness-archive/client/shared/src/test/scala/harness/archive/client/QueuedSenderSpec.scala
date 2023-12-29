package harness.archive.client

import cats.syntax.option.*
import harness.zio.test.DefaultHarnessSpec
import zio.*
import zio.test.*
import zio.test.Assertion.*

object QueuedSenderSpec extends DefaultHarnessSpec {

  private def makeSender[A](chunkSize: Int, dumpEvery: Option[Duration]): URIO[Scope, (Ref[List[Chunk[A]]], QueuedSender[Any, A])] =
    for {
      ref <- Ref.make(List.empty[Chunk[A]])
      sender <- QueuedSender.make[Any, A]("test", chunkSize, dumpEvery, false) { chunk =>
        ref.update { chunk :: _ }
      }
    } yield (ref, sender)

  override def spec: TestSpec =
    suite("QueuedSenderSpec")(
      test("Case 1") {
        for {
          (ref, sender) <- makeSender[Int](3, None)
          _ <- sender.push(1)
          _ <- sender.push(2)
          refS1 <- ref.get
          _ <- sender.push(3)
          refS2 <- ref.get
        } yield assertTrue(
          refS1 == Nil,
          refS2 == List(Chunk(1, 2, 3)),
        )
      },
      test("Case 2") {
        for {
          (ref, sender) <- makeSender[Int](3, None)
          _ <- sender.pushAll(List(1, 2))
          refS1 <- ref.get
          _ <- sender.pushAll(List(3, 4))
          refS2 <- ref.get
        } yield assertTrue(
          refS1 == Nil,
          refS2 == List(Chunk(1, 2, 3, 4)),
        )
      },
      test("Case 3") {
        for {
          (ref, sender) <- makeSender[Int](3, 3.minutes.some)
          _ <- TestClock.adjust(1.minute)
          _ <- sender.pushAll(List(1, 2))
          refS1 <- ref.get
          _ <- TestClock.adjust(1.minute)
          refS2 <- ref.get
          _ <- TestClock.adjust(1.minute)
          refS3 <- ref.get
        } yield assertTrue(
          refS1 == Nil,
          refS2 == Nil,
          refS3 == List(Chunk(1, 2)),
        )
      },
      test("Case 4") {
        for {
          (ref, refS1) <- ZIO.scoped {
            for {
              (ref, sender) <- makeSender[Int](3, None)
              _ <- sender.pushAll(List(1, 2))
              refS1 <- ref.get
            } yield (ref, refS1)
          }
          refS2 <- ref.get
        } yield assertTrue(
          refS1 == Nil,
          refS2 == List(Chunk(1, 2)),
        )
      },
    )

}
