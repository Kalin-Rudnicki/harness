package harness.sql.mock

import harness.zio.*
import monocle.Lens
import monocle.macros.GenLens
import zio.*

final class MockState[E, A](private[mock] val ref: Ref.Synchronized[A])(implicit errorMapper: ErrorMapper[Throwable, E]) {

  val get: UIO[A] = ref.get

  def getWith[B](f: A => Task[B]): IO[E, B] =
    ref.get.flatMap(f(_).mapError(errorMapper.mapError))

  def update(f: A => Task[A]): IO[E, Unit] =
    ref.updateZIO { f(_).mapError(errorMapper.mapError) }

  inline def focusAndUpdate[B](inline focus: A => B)(f: B => Task[B]): IO[E, Unit] = {
    val lens: Lens[A, B] = GenLens[A](focus).asInstanceOf[Lens[A, B]]
    update { a => f(lens.get(a)).map(lens.replace(_)(a)) }
  }

  inline def focusAndUpdateW[B](inline focus: A => B)(f: (A, B) => Task[B]): IO[E, Unit] = {
    val lens: Lens[A, B] = GenLens[A](focus).asInstanceOf[Lens[A, B]]
    update { a => f(a, lens.get(a)).map(lens.replace(_)(a)) }
  }

}
object MockState {

  def make[E, A](a: A)(implicit errorMapper: ErrorMapper[Throwable, E]): UIO[MockState[E, A]] =
    Ref.Synchronized.make(a).map(new MockState[E, A](_))

  def layer[E: Tag, A: Tag](a: A)(implicit errorMapper: ErrorMapper[Throwable, E]): ULayer[MockState[E, A]] =
    ZLayer.fromZIO { MockState.make[E, A](a) }

}
