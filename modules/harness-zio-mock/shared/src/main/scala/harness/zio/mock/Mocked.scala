package harness.zio.mock

import zio.*

final class Mocked[Z](
    private[Mocked] val impls: List[Expectation[? >: Z, ?, ?, ?, ?]],
    private[Mocked] val mocks: Set[Mock[? >: Z]],
) { self =>

  final def ++[Z2](that: Mocked[Z2]): Mocked[Z & Z2] = {
    val _self: Mocked[Z & Z2] = self.asInstanceOf
    val _that: Mocked[Z & Z2] = that.asInstanceOf
    new Mocked[Z & Z2](
      impls = _self.impls ::: _that.impls,
      mocks = _self.mocks | _that.mocks,
    )
  }

  final def toLayer: URLayer[Proxy, Z] =
    ZLayer.fromZIOEnvironment {
      for {
        // get proxy
        proxy <- ZIO.service[Proxy]

        // add impls to proxy
        _ <- ZIO.foreachDiscard(impls)(i => proxy.putImpl(i.capability, i.effect))

        // create env
        zEnvs: List[ZEnvironment[?]] =
          self.mocks.toList.map { _.build(proxy) }
        zEnv: ZEnvironment[Z] =
          zEnvs.foldLeft(ZEnvironment.empty) { _ ++ _ }.asInstanceOf[ZEnvironment[Z]]
      } yield zEnv
    }

}
object Mocked {

  private[mock] def single[Z, I, R, E, A](capability: Mock[Z]#Capability[I, R, E, A], effect: I => ZIO[R, E, A]): Mocked[Z] =
    new Mocked[Z](
      Expectation(capability, effect) :: Nil,
      Set(capability.getMock),
    )

  private[mock] def empty[Z](mock: Mock[Z]): Mocked[Z] =
    new Mocked[Z](
      Nil,
      Set(mock),
    )

}
