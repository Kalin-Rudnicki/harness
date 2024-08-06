package harness.zio

import java.util.UUID
import zio.*

object TestMain extends ExecutableApp {

  final case class MyThing private (id: UUID) {

    def use: UIO[Unit] = Logger.log.trace("use", "id" -> id)

    def close: UIO[Unit] = Logger.log.trace("--- close ---", "id" -> id)

  }
  object MyThing {

    def make(counter: Ref[Int]): URIO[Scope, MyThing] =
      for {
        _ <- counter.update(_ + 1)
        id <- Random.nextUUID
        myThing <- ZIO.succeed { new MyThing(id) }.withFinalizer(_.close)
        _ <- Logger.log.trace("make", "id" -> id)
      } yield myThing

  }

  final class WrappedZPool[+E, A](wrapped: ZPool[E, A], show: A => String) {

    def get: ZIO[Scope, E, A] =
      for {
        a <- wrapped.get
        _ <- Logger.log.trace(s"Acquiring: ${show(a)}")
        _ <- ZIO.addFinalizer(Logger.log.trace(s"Releasing: ${show(a)}"))
      } yield a

  }

  override val executable: Executable =
    Executable.implement {
      for {
        counter <- Ref.make(0)
        _ <- Logger.log.info("Starting...")
        rawPool <- ZPool.make(MyThing.make(counter), 0 to 4, 1.second)
        pool = new WrappedZPool[Nothing, MyThing](rawPool, _.id.toString)

        effect = ZIO.scoped { (pool.get <* Clock.sleep(100.millis)).flatMap(_.use) *> Clock.sleep(100.millis) }

        _ <- ZIO.collectAllParDiscard(Chunk.fill(10)(effect))
        _ <- Clock.sleep(5.seconds)
        _ <- ZIO.collectAllParDiscard(Chunk.fill(10)(effect))

        count <- counter.get
        _ <- Logger.log.info(s"count = $count")

        path <- Path("abc.scala")
        _ <- path.writeString("hello file")
      } yield ()
    }

}
