package harness.archive.client

import cats.syntax.option.*
import zio.*

final class QueuedSender[R, A] private (
    name: String,
    send: Chunk[A] => URIO[R, Unit],
    chunkSize: Int,
    forked: Boolean,
    ref: Ref[List[A]],
    open: Ref[Boolean],
) {

  private inline def forkIf(effect: URIO[R, Unit]): URIO[R, Unit] =
    if (forked) effect.fork.unit
    else effect

  def push(value: A): URIO[R, Unit] =
    forkIf {
      checkOpen *>
        ref
          .modify { list =>
            val newList = value :: list
            if (newList.size >= chunkSize) (newList.reverse.some, Nil)
            else (None, newList)
          }
          .flatMap {
            ZIO.foreachDiscard(_) { list => send(Chunk.fromIterable(list)) }
          }
    }

  def pushAll(values: List[A]): URIO[R, Unit] =
    forkIf {
      checkOpen *>
        ref
          .modify { list =>
            val newList = values.reverse ::: list
            if (newList.size >= chunkSize) (newList.reverse.some, Nil)
            else (None, newList)
          }
          .flatMap {
            ZIO.foreachDiscard(_) { list => send(Chunk.fromIterable(list)) }
          }
    }

  def dump: URIO[R, Unit] =
    checkOpen *>
      ref
        .modify { list => (list.reverse, Nil) }
        .flatMap { list =>
          ZIO.foreachDiscard(Option.when(list.nonEmpty)(list)) { list => send(Chunk.fromIterable(list)) }
        }

  private def checkOpen: UIO[Unit] =
    ZIO.dieMessage(s"QueuedSender '$name' is closed").unlessZIO(open.get).unit

  private def close: URIO[R, Unit] =
    dump *> open.set(false)

}
object QueuedSender {

  /**
    * Make sure this is not called outside the `Scope` from which it is returned.
    * (Or it will die)
    */
  def make[R, A](
      name: String,
      chunkSize: Int,
      dumpEvery: Option[Duration],
      forked: Boolean,
  )(send: Chunk[A] => URIO[R, Unit]): URIO[R & Scope, QueuedSender[R, A]] =
    for {
      ref <- Ref.make(List.empty[A])
      open <- Ref.make(true)
      sender <- ZIO
        .succeed(new QueuedSender[R, A](name, send, chunkSize, forked, ref, open))
        .withFinalizer { _.close }
      _ <- ZIO.foreachDiscard(dumpEvery) { dumpEvery =>
        (
          Clock.sleep(dumpEvery) *>
            sender.dump.repeat(Schedule.spaced(dumpEvery)),
        ).interruptible.fork.withFinalizer { _.interrupt }
      }
    } yield sender

}
