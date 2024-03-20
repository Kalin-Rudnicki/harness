package harness.zio.mock

import java.util.UUID
import zio.*

abstract class Mock[Z](implicit rTag: Tag[Z]) { myMock =>

  private[mock] final val mockId: UUID = UUID.randomUUID

  override final def hashCode: Int = mockId.hashCode

  override final def equals(that: Any): Boolean =
    that.asInstanceOf[Matchable] match {
      case that: Mock[?] => this.mockId == that.mockId
      case _             => false
    }

  final def name: String =
    myMock.getClass.getSimpleName.stripSuffix("$")

  abstract class Capability[I, R, E, A] { myCapability =>

    private[mock] final val capabilityId: UUID = UUID.randomUUID

    override final def hashCode: Int = capabilityId.hashCode

    override final def equals(that: Any): Boolean =
      that.asInstanceOf[Matchable] match {
        case that: Capability[?, ?, ?, ?] => this.capabilityId == that.capabilityId
        case _                            => false
      }

    final def name: String =
      s"${myMock.name}<${myCapability.getClass.getSimpleName.stripSuffix("$")}>"

    // =====|  |=====

    private[mock] def getMock: Mock[Z] = myMock

    // =====|  |=====

    /**
      * Use these when creating the mocked layer.
      * It allows you to specify how the capability should be implemented.
      * These differ from seeded expectations,
      * in that implementations don't care what order they are called in,
      * or if they are called at all.
      */
    object implement {

      def zioI(f: I => ZIO[R, E, A]): Mocked[Z] =
        Mocked.single(myCapability, f)

      def zio(f: => ZIO[R, E, A]): Mocked[Z] =
        zioI(_ => f)

      def successI(f: I => A): Mocked[Z] =
        zioI(i => ZIO.succeed(f(i)))

      def success(f: => A): Mocked[Z] =
        zioI(_ => ZIO.succeed(f))

      def failureI(f: I => E): Mocked[Z] =
        zioI(i => ZIO.fail(f(i)))

      def failure(f: => E): Mocked[Z] =
        zioI(_ => ZIO.fail(f))

    }

    /**
      * Use these to seed expected calls to the capability.
      * It is very important to note that ordering is enforced.
      * If you seed:
      * - `MyMock.Capability1.seed.success(1)`
      * - `MyMock.Capability2.seed.success("test")`
      * and then call `capability2()` before calling `capability1()`,
      * ZIO will die with an error letting you know that a call to `capability2()` is expected next.
      */
    object seed {

      // append

      def zioI(f: I => ZIO[R, E, A]): URIO[Proxy & Z, Unit] =
        ZIO.serviceWithZIO[Proxy](_.appendSeed(myCapability, f))

      def zio(f: => ZIO[R, E, A]): URIO[Proxy & Z, Unit] =
        zioI(_ => f)

      def successI(f: I => A): URIO[Proxy & Z, Unit] =
        zioI(i => ZIO.succeed(f(i)))

      def success(f: => A): URIO[Proxy & Z, Unit] =
        zioI(_ => ZIO.succeed(f))

      def failureI(f: I => E): URIO[Proxy & Z, Unit] =
        zioI(i => ZIO.fail(f(i)))

      def failure(f: => E): URIO[Proxy & Z, Unit] =
        zioI(_ => ZIO.fail(f))

      object prepend {

        def zioI(f: I => ZIO[R, E, A]): URIO[Proxy & Z, Unit] =
          ZIO.serviceWithZIO[Proxy](_.prependSeed(myCapability, f))

        def zio(f: => ZIO[R, E, A]): URIO[Proxy & Z, Unit] =
          zioI(_ => f)

        def successI(f: I => A): URIO[Proxy & Z, Unit] =
          zioI(i => ZIO.succeed(f(i)))

        def success(f: => A): URIO[Proxy & Z, Unit] =
          zioI(_ => ZIO.succeed(f))

        def failureI(f: I => E): URIO[Proxy & Z, Unit] =
          zioI(i => ZIO.fail(f(i)))

        def failure(f: => E): URIO[Proxy & Z, Unit] =
          zioI(_ => ZIO.fail(f))

      }

      object append {

        def zioI(f: I => ZIO[R, E, A]): URIO[Proxy & Z, Unit] =
          ZIO.serviceWithZIO[Proxy](_.appendSeed(myCapability, f))

        def zio(f: => ZIO[R, E, A]): URIO[Proxy & Z, Unit] =
          zioI(_ => f)

        def successI(f: I => A): URIO[Proxy & Z, Unit] =
          zioI(i => ZIO.succeed(f(i)))

        def success(f: => A): URIO[Proxy & Z, Unit] =
          zioI(_ => ZIO.succeed(f))

        def failureI(f: I => E): URIO[Proxy & Z, Unit] =
          zioI(i => ZIO.fail(f(i)))

        def failure(f: => E): URIO[Proxy & Z, Unit] =
          zioI(_ => ZIO.fail(f))

      }

    }

  }

  abstract class Effect[I, E, A] extends Capability[I, Any, E, A]

  // =====|  |=====

  def empty: Mocked[Z] = Mocked.empty[Z](myMock)

  protected def buildInternal(proxy: Proxy): Z

  private[mock] final def build(proxy: Proxy): ZEnvironment[Z] = ZEnvironment(buildInternal(proxy))

}
