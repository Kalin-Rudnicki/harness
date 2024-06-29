package harness.zio.mock

import harness.zio.*
import harness.zio.mock.error.MockError
import harness.zio.test.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

object MockSpec extends DefaultHarnessSpec {

  private trait ExService {
    def funct1(i: Int): IO[String, Int]
    def funct2(i1: Int, i2: Double): IO[String, Double]
    def funct3(s: String): IO[String, Int]
  }
  private object ExService {
    def funct1(i: Int): ZIO[ExService, String, Int] =
      ZIO.serviceWithZIO[ExService](_.funct1(i))
    def funct2(i1: Int, i2: Double): ZIO[ExService, String, Double] =
      ZIO.serviceWithZIO[ExService](_.funct2(i1, i2))
    def funct3(s: String): ZIO[ExService, String, Int] =
      ZIO.serviceWithZIO[ExService](_.funct3(s))
  }

  private object ExServiceMock extends Mock[ExService] {

    object Funct1 extends Effect[Int, String, Int]
    object Funct2 extends Effect[(Int, Double), String, Double]
    object Funct3 extends Effect[String, String, Int]

    override protected def buildInternal(proxy: Proxy): ExService =
      new ExService {
        override def funct1(i: Int): IO[String, Int] =
          proxy(Funct1, i)
        override def funct2(i1: Int, i2: Double): IO[String, Double] =
          proxy(Funct2, i1, i2)
        override def funct3(s: String): IO[String, Int] =
          proxy(Funct3, s)
      }

  }

  private def makeTest(name: String)(mocked: Mocked[ExService])(testF: => ZIO[HarnessEnv & ExService & Proxy, Any, TestResult]): TestSpec =
    test(name) { testF.provideSomeLayer[HarnessEnv](Proxy.layer >+> mocked.toLayer) }

  private def makeSeedTest(name: String)(testF: => ZIO[HarnessEnv & ExService & Proxy, Any, TestResult]): Spec[HarnessEnv & ExService & Proxy, Any] =
    test(name) { testF }

  private val positiveImplSpec: TestSpec =
    suite("positive")(
      makeTest("impl can be called 0 times") {
        ExServiceMock.Funct1.implement.success(1)
      } {
        assertCompletes
      },
      makeTest("impl can be called 1 time1") {
        ExServiceMock.Funct1.implement.success(1)
      } {
        for {
          res1 <- ExService.funct1(0)
        } yield assertTrue(
          res1 == 1,
        )
      },
      makeTest("impl can be called many times") {
        ExServiceMock.Funct1.implement.success(1)
      } {
        for {
          res1 <- ExService.funct1(0)
          res2 <- ExService.funct1(1)
          res3 <- ExService.funct1(2)
        } yield assertTrue(
          res1 == 1,
          res2 == 1,
          res3 == 1,
        )
      },
      makeTest("ordering doesn't matter (1)") {
        ExServiceMock.Funct1.implement.successI { _ * 2 } ++
          ExServiceMock.Funct2.implement.successI { case (a, b) => a * b }
      } {
        for {
          res1 <- ExService.funct1(1)
          res2 <- ExService.funct2(2, 3)
          res3 <- ExService.funct1(4)
          res4 <- ExService.funct2(5, 6)
        } yield assertTrue(
          res1 == 2,
          res2 == 6,
          res3 == 8,
          res4 == 30,
        )
      },
      makeTest("ordering doesn't matter (2)") {
        ExServiceMock.Funct1.implement.successI { _ * 2 } ++
          ExServiceMock.Funct2.implement.successI { case (a, b) => a * b }
      } {
        for {
          res1 <- ExService.funct2(1, 2)
          res2 <- ExService.funct1(3)
          res3 <- ExService.funct2(4, 5)
          res4 <- ExService.funct1(6)
        } yield assertTrue(
          res1 == 2,
          res2 == 6,
          res3 == 20,
          res4 == 12,
        )
      },
    )

  private val positiveSeedSpec: TestSpec =
    suite("positive")(
      makeSeedTest("allows creation and consumption of seeded expectations") {
        for {
          _ <- ExServiceMock.Funct1.seed.success(1)
          _ <- ExServiceMock.Funct2.seed.success(2)
          _ <- ExServiceMock.Funct1.seed.successI(_ + 100).replicateZIODiscard(2)

          res1 <- ExService.funct1(0)
          res2 <- ExService.funct2(0, 0)
          res3 <- ExService.funct1(0)
          res4 <- ExService.funct1(25)
        } yield assertTrue(
          res1 == 1,
          res2 == 2,
          res3 == 100,
          res4 == 125,
        )
      },
    ).provideSomeLayer[HarnessEnv](Proxy.layer >+> ExServiceMock.empty.toLayer)

  private val negativeSeedSpec: TestSpec =
    suite("negative")(
      makeTest("fails if seeds are left unconsumed at end of test") {
        ExServiceMock.empty
      } {
        for {
          _ <- ExServiceMock.Funct1.seed.success(1)
          _ <- ExServiceMock.Funct2.seed.success(1)
        } yield assertCompletes
      } @@ TestAspect.failing {
        case TestFailure.Runtime(Cause.Die(_: MockError.UnsatisfiedCalls, _), _) => true
        case _                                                                   => false
      },
      makeTest("fails if there are no seeds") {
        ExServiceMock.empty
      } {
        for {
          res <- ExService.funct1(0).exit
        } yield assert(res)(dies(equalTo(MockError.UnexpectedCall(ExServiceMock.Funct1, Nil))))
      },
      makeTest("fails if wrong seed is called") {
        ExServiceMock.empty
      } {
        for {
          _ <- ExServiceMock.Funct2.seed.success(0)
          res <- ExService.funct1(0).exit
          _ <- ExService.funct2(0, 0) // empty seed
        } yield assert(res)(dies(isSubtype[MockError.UnexpectedCall](anything)))
      },
    )

  private val positiveImplAndSeedSpec: TestSpec =
    suite("positive")(
      makeTest("Prefers seeded expectation to impl if that is the next expected call") {
        ExServiceMock.Funct1.implement.successI { _ * 2 }
      } {
        for {
          res1 <- ExService.funct1(1)
          _ <- ExServiceMock.Funct1.seed.success(10)
          res2 <- ExService.funct1(2)
          res3 <- ExService.funct1(3)
        } yield assertTrue(
          res1 == 2,
          res2 == 10,
          res3 == 6,
        )
      },
    )

  private val positiveAsyncSpec: TestSpec =
    suite("positive")(
      makeSeedTest("allows out of order calls - 1") {
        for {
          _ <- ExServiceMock.Funct1.seed.success(1)
          _ <- ExServiceMock.Funct2.seed.async.success(2)
          _ <- ExServiceMock.Funct3.seed.async.success(3)
          _ <- ExServiceMock.Funct1.seed.success(4)

          res1 <- ExService.funct1(0)
          res2 <- ExService.funct2(0, 0)
          res3 <- ExService.funct3("")
          res4 <- ExService.funct1(0)
        } yield assertTrue(
          res1 == 1,
          res2 == 2,
          res3 == 3,
          res4 == 4,
        )
      },
      makeSeedTest("allows out of order calls - 2") {
        for {
          _ <- ExServiceMock.Funct1.seed.success(1)
          _ <- ExServiceMock.Funct2.seed.async.success(2)
          _ <- ExServiceMock.Funct3.seed.async.success(3)
          _ <- ExServiceMock.Funct1.seed.success(4)

          res1 <- ExService.funct1(0)
          res3 <- ExService.funct3("")
          res2 <- ExService.funct2(0, 0)
          res4 <- ExService.funct1(0)
        } yield assertTrue(
          res1 == 1,
          res2 == 2,
          res3 == 3,
          res4 == 4,
        )
      },
    ).provideSomeLayer[HarnessEnv](Proxy.layer >+> ExServiceMock.empty.toLayer)

  override def testSpec: TestSpec =
    suite("ExServiceSpec")(
      suite("impl")(
        positiveImplSpec,
      ),
      suite("seed")(
        positiveSeedSpec,
        negativeSeedSpec,
      ),
      suite("impl + seed")(
        positiveImplAndSeedSpec,
      ),
      suite("async seed")(
        positiveAsyncSpec,
      ),
    )

}
