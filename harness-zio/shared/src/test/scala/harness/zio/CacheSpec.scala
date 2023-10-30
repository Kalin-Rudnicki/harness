package harness.zio

import cats.syntax.option.*
import harness.core.*
import harness.zio.test.DefaultHarnessSpec
import zio.*
import zio.test.*
import zio.test.Assertion.*

object CacheSpec extends DefaultHarnessSpec {

  override lazy val logLevel: Logger.LogLevel = Logger.LogLevel.Detailed

  override def spec: TestSpec =
    suite("CacheSpec")(
      test("check on missing value is None") {
        for {
          cache <- Cache.make[Int, String]("cache", None)
          k = 0
          value <- cache.check(k)
        } yield assert(value)(isNone)
      },
      test("get on missing value runs effect") {
        for {
          cache <- Cache.make[Int, String]("cache", None)
          k = 0
          v = "value"
          value <- cache.get(k)(_ => ZIO.succeed(v))
        } yield assert(value)(equalTo(v))
      },
      test("put + check works") {
        for {
          cache <- Cache.make[Int, String]("cache", None)
          k = 0
          v = "value"
          _ <- cache.put(k, v)
          value <- cache.check(k)
        } yield assert(value)(isSome(equalTo(v)))
      },
      test("get on existing value does not run effect") {
        for {
          cache <- Cache.make[Int, String]("cache", None)
          k = 0
          v = "value"
          _ <- cache.put(k, v)
          value <- cache.getLogged(k)(_ => ZIO.fail(HError.InternalDefect("value is missing")))
        } yield assert(value)(equalTo(v))
      },
      test("get misses and then hits") {
        for {
          cache <- Cache.make[Int, String]("cache", None)
          k = 0
          v = "value"
          ref <- Ref.make(v)
          getValueFromCache = cache.getLogged(k)(_ => ref.get)
          value1 <- getValueFromCache
          _ <- ref.set("something else")
          value2 <- getValueFromCache
        } yield assert(value1)(equalTo(v)) && assert(value2)(equalTo(v))
      },
      test("check on expired value is None") {
        for {
          cache <- Cache.make[Int, String]("cache", 1.second.some)
          k = 0
          v = "value"
          _ <- cache.put(k, v)
          _ <- TestClock.adjust(1.second)
          value <- cache.check(k)
        } yield assert(value)(isNone)
      },
      test("check on not expired value is Some") {
        for {
          cache <- Cache.make[Int, String]("cache", 2.seconds.some)
          k = 0
          v = "value"
          _ <- cache.put(k, v)
          _ <- TestClock.adjust(1.second)
          value <- cache.check(k)
        } yield assert(value)(isSome(equalTo(v)))
      },
      test("toString") {
        for {
          cache <- Cache.make[Int, String]("cache", None)
        } yield assert(cache.toString)(equalTo("Cache[Int, String](cache, None)"))
      },
      test("invalidateExpired") {
        for {
          cache <- Cache.make[Int, String]("cache", 2.seconds.some)
          _ <- cache.put(0, "v0")
          _ <- TestClock.adjust(1.second)
          _ <- cache.put(1, "v1")
          _ <- TestClock.adjust(1.second)
          v0Before <- cache.isCached(0)
          v1Before <- cache.isCached(1)
          _ <- cache.invalidateExpired
          v0After <- cache.isCached(0)
          v1After <- cache.isCached(1)
        } yield assert(v0Before)(isTrue) && assert(v1Before)(isTrue) && assert(v0After)(isFalse) && assert(v1After)(isTrue)
      },
    )

}
