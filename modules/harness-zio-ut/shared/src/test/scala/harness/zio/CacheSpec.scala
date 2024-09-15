package harness.zio

import harness.zio.test.*
import zio.*
import zio.test.*
import zio.test.Assertion.*

object CacheSpec extends DefaultHarnessSpec {

  override def testSpec: TestSpec =
    suite("CacheSpec")(
      test("check on missing value is None") {
        for {
          cache <- Cache.make[Int, String]("cache")
          k = 0
          value <- cache.check(k)
        } yield assert(value)(isNone)
      },
      test("get on missing value runs effect") {
        for {
          cache <- Cache.make[Int, String]("cache")
          k = 0
          v = "value"
          value <- cache.getOrFetch(k)(_ => ZIO.succeed(v))
        } yield assert(value)(equalTo(v))
      },
      test("put + check works") {
        for {
          cache <- Cache.make[Int, String]("cache")
          k = 0
          v = "value"
          _ <- cache.put(k, v)
          value <- cache.check(k)
        } yield assert(value)(isSome(equalTo(v)))
      },
      test("get on existing value does not run effect") {
        for {
          cache <- Cache.make[Int, String]("cache")
          k = 0
          v = "value"
          _ <- cache.put(k, v)
          value <- cache.getOrFetch(k)(_ => ZIO.dieMessage("value is missing"))
        } yield assert(value)(equalTo(v))
      },
      test("get misses and then hits") {
        for {
          cache <- Cache.make[Int, String]("cache")
          k = 0
          v = "value"
          ref <- Ref.make(v)
          getValueFromCache = cache.getOrFetch(k)(_ => ref.get)
          value1 <- getValueFromCache
          _ <- ref.set("something else")
          value2 <- getValueFromCache
        } yield assert(value1)(equalTo(v)) && assert(value2)(equalTo(v))
      },
      test("check on expired value is None") {
        for {
          cache <- Cache.make[Int, String]("cache", 1.second)
          k = 0
          v = "value"
          _ <- cache.put(k, v)
          _ <- TestClock.adjust(1.second)
          value <- cache.check(k)
        } yield assert(value)(isNone)
      },
      test("check on not expired value is Some") {
        for {
          cache <- Cache.make[Int, String]("cache", 2.seconds)
          k = 0
          v = "value"
          _ <- cache.put(k, v)
          _ <- TestClock.adjust(1.second)
          value <- cache.check(k)
        } yield assert(value)(isSome(equalTo(v)))
      },
      test("toString") {
        for {
          cache <- Cache.make[Int, String]("cache")
        } yield assert(cache.toString)(equalTo("Cache[Int, String](name = cache)"))
      },
      test("invalidateExpired") {
        for {
          cache <- Cache.make[Int, String]("cache", 2.seconds)
          _ <- cache.put(0, "v0")
          _ <- TestClock.adjust(1.second)
          _ <- cache.put(1, "v1")
          _ <- TestClock.adjust(1.second)
          v0T1 <- cache.isCached(0)
          v1T1 <- cache.isCached(1)
          _ <- cache.invalidateExpired
          v0T2 <- cache.isCached(0)
          v1T2 <- cache.isCached(1)
        } yield assertTrue(
          v0T1,
          v1T1,
          !v0T2,
          v1T2,
        )
      },
      test("schedule auto clears") {
        for {
          cache <- Cache.make[Int, String]("cache", 2.seconds, Schedule.fromDurationsForever(2.seconds))
          _ <- cache.put(0, "v0")
          _ <- TestClock.adjust(1.second)
          _ <- cache.put(1, "v1")
          v0T1 <- cache.isCached(0)
          v1T1 <- cache.isCached(1)
          _ <- TestClock.adjust(1.second)
          v0T2 <- cache.isCached(0)
          v1T2 <- cache.isCached(1)
        } yield assertTrue(
          v0T1,
          v1T1,
          !v0T2,
          v1T2,
        )
      },
    )

}
