package harness.zio

import cats.syntax.option.*
import java.time.Instant
import scala.math.Ordering.Implicits.infixOrderingOps
import zio.*

sealed trait Cache[K, V] {

  /**
    * Returns whether the given key is currently cached.
    * If it is, returns true, whether or not the value for that key is expired.
    */
  def isCached(k: K): UIO[Boolean]

  /**
    * Optionally gets the value from the cache.
    * If the given key has a value, but the value is expired, None is returned.
    */
  def check(k: K, logLevel: Logger.LogLevel = Logger.LogLevel.Debug): UIO[Option[V]]

  /**
    * Attempts to get a value from the cache for the given key.
    * If there is not a value for that key, or the value for that key is expired, use the provided function to calculate a new value.
    */
  def getOrFetch[R, E](k: K, logLevel: Logger.LogLevel = Logger.LogLevel.Debug)(_get: K => ZIO[R, E, V]): ZIO[R, E, V]
  inline final def apply[R, E](k: K, logLevel: Logger.LogLevel = Logger.LogLevel.Debug)(_get: K => ZIO[R, E, V]): ZIO[R, E, V] = getOrFetch(k, logLevel)(_get)

  /**
    * Manually adds the given key-value pair to the cache.
    */
  def put(k: K, v: V): UIO[Unit]

  /**
    * Manually adds all the given key-value pairs to the cache.
    */
  def putAll(pairs: (K, V)*): UIO[Unit]

  /**
    * Manually adds the given key-value pair that results from the provided function.
    */
  def cache[R, E](k: K)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit]

  /**
    * Manually adds all the given key-value pairs that result from the provided function.
    */
  def cacheAll[R, E](ks: K*)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit]

  /**
    * Manually adds all the given key-value pairs that result from the provided function. (parallel)
    */
  def cacheAllPar[R, E](ks: K*)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit]

  /**
    * Manually removes the given key from the cache.
    */
  def invalidate(k: K): UIO[Unit]

  /**
    * Manually removes all the given keys from the cache.
    */
  def invalidateAll(ks: K*): UIO[Unit]

  /**
    * Manually empties the entire cache.
    */
  def invalidateAll: UIO[Unit]

  /**
    * Manually removes values from the cache which satisfy the provided function.
    */
  def invalidateValues(f: V => Boolean): UIO[Unit]

  /**
    * Removes item from the cache which are expired. Returns how many items where removed.
    */
  def invalidateExpired: UIO[Int]

  /**
    * For all items currently cached, recalculate their values using the provided function.
    */
  def refreshAll[R, E](_get: K => ZIO[R, E, V]): ZIO[R, E, Unit]

  /**
    * For all items currently cached, recalculate their values using the provided function. (parallel)
    */
  def refreshAllPar[R, E](_get: K => ZIO[R, E, V]): ZIO[R, E, Unit]

  def dumpState(
      logLevel: Logger.LogLevel = Logger.LogLevel.Info,
      showKey: K => String = (_: K).toString,
      showValue: V => String = (_: V).toString,
  ): UIO[Unit]

}
object Cache {

  // =====|  |=====

  def make[K: HTag, V: HTag](name: String, expireDuration: Option[Duration], initialValues: (K, V)*): UIO[Cache[K, V]] = expireDuration match
    case Some(expireDuration) => Cache.make[K, V](name, expireDuration, initialValues*)
    case None                 => Cache.make[K, V](name, initialValues*)

  /**
    * Creates a cache where key-value pairs never expire.
    */
  def make[K: HTag, V: HTag](name: String, initialValues: (K, V)*): UIO[Cache[K, V]] =
    Ref.Synchronized
      .make(initialValues.toMap)
      .map(Cache.WithoutExpiration(_, name, HTag[K], HTag[V]))

  /**
    * Creates a cache where key-value pairs expire after the provided duration.
    */
  def make[K: HTag, V: HTag](name: String, expireDuration: Duration, initialValues: (K, V)*): UIO[Cache[K, V]] =
    Clock.instant.flatMap { now =>
      val exp = now.plus(expireDuration)
      Ref.Synchronized
        .make(initialValues.map { case (k, v) => (k, (exp, v)) }.toMap)
        .map(Cache.WithExpiration(_, expireDuration, name, HTag[K], HTag[V]))
    }

  // TODO (KR) : support schedule with R type
  /**
    * Creates a cache where key-value pairs expire after the provided duration.
    * On the provided schedule, expired items will be removed (for as long as the scope is open).
    */
  def make[K: HTag, V: HTag](name: String, expireDuration: Duration, clearSchedule: Schedule[Any, Boolean, Any], initialValues: (K, V)*): URIO[Scope, Cache[K, V]] =
    for {
      cache <- Cache.make[K, V](name, expireDuration, initialValues*)
      _ <- cache.invalidateExpired.map(_ > 0).repeat(clearSchedule).forkScoped
    } yield cache

  // =====|  |=====

  def layer[K: Tag, V: Tag](name: String, expireDuration: Option[Duration], initialValues: (K, V)*): ULayer[Cache[K, V]] =
    ZLayer { Cache.make[K, V](name, expireDuration, initialValues*) }

  /**
    * Creates a cache where key-value pairs never expire.
    */
  def layer[K: Tag, V: Tag](name: String, initialValues: (K, V)*): ULayer[Cache[K, V]] =
    ZLayer { Cache.make[K, V](name, initialValues*) }

  /**
    * Creates a cache where key-value pairs expire after the provided duration.
    */
  def layer[K: Tag, V: Tag](name: String, expireDuration: Duration, initialValues: (K, V)*): ULayer[Cache[K, V]] =
    ZLayer { Cache.make[K, V](name, expireDuration, initialValues*) }

  // TODO (KR) : support schedule with R type
  /**
    * Creates a cache where key-value pairs expire after the provided duration.
    * On the provided schedule, expired items will be removed (for as long as the scope is open).
    */
  def layer[K: Tag, V: Tag](name: String, expireDuration: Duration, clearSchedule: Schedule[Any, Boolean, Any], initialValues: (K, V)*): ULayer[Cache[K, V]] =
    ZLayer.scoped { Cache.make[K, V](name, expireDuration, clearSchedule, initialValues*) }

  // =====|  |=====

  object unsafe {

    def make[K: HTag, V: HTag](name: String, expireDuration: Option[Duration], initialValues: (K, V)*)(implicit unsafe: Unsafe): Cache[K, V] = expireDuration match
      case Some(expireDuration) => Cache.unsafe.make[K, V](name, expireDuration, initialValues*)
      case None                 => Cache.unsafe.make[K, V](name, initialValues*)

    /**
      * Creates a cache where key-value pairs never expire.
      */
    def make[K: HTag, V: HTag](name: String, initialValues: (K, V)*)(implicit unsafe: Unsafe): Cache[K, V] = {
      val ref = Ref.Synchronized.unsafe.make(initialValues.toMap)
      Cache.WithoutExpiration(ref, name, HTag[K], HTag[V])
    }

    /**
      * Creates a cache where key-value pairs expire after the provided duration.
      */
    def make[K: HTag, V: HTag](name: String, expireDuration: Duration, initialValues: (K, V)*)(implicit unsafe: Unsafe): Cache[K, V] = {
      val exp = Instant.now().plus(expireDuration)
      val ref = Ref.Synchronized.unsafe.make(initialValues.map { case (k, v) => (k, (exp, v)) }.toMap)
      Cache.WithExpiration(ref, expireDuration, name, HTag[K], HTag[V])
    }

  }

  // =====|  |=====

  private final class WithoutExpiration[K, V](
      ref: Ref.Synchronized[Map[K, V]],
      name: String,
      keyTag: HTag[K],
      valueTag: HTag[V],
  ) extends Cache[K, V] {

    override def isCached(k: K): UIO[Boolean] =
      ref.get.map(_.contains(k))

    override def check(k: K, logLevel: Logger.LogLevel): UIO[Option[V]] =
      ref.get.map(_.get(k)).tap { res => Logger.log(logLevel)("cache:check", "cache-name" -> name, "cache-result" -> (if (res.nonEmpty) "hit" else "miss")) }

    override def getOrFetch[R, E](k: K, logLevel: Logger.LogLevel)(_get: K => ZIO[R, E, V]): ZIO[R, E, V] =
      ref.modifyZIO { cache =>
        cache.get(k) match {
          case Some(v) => Logger.log(logLevel)("cache:get-or-fetch", "cache-name" -> name, "cache-result" -> "hit").as((v, cache))
          case None    => Logger.log(logLevel)("cache:get-or-fetch", "cache-name" -> name, "cache-result" -> "miss") *> _get(k).map { v => (v, cache.updated(k, v)) }
        }
      }

    override def put(k: K, v: V): UIO[Unit] =
      ref.update(_.updated(k, v))

    override def putAll(pairs: (K, V)*): UIO[Unit] =
      ref.update(_ ++ pairs)

    override def cache[R, E](k: K)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
      ref.updateZIO { cache =>
        _get(k).map { v => cache.updated(k, v) }
      }

    override def cacheAll[R, E](ks: K*)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
      ref.updateZIO { cache =>
        ZIO.foreach(ks) { k => _get(k).map((k, _)) }.map { cache ++ _ }
      }

    override def cacheAllPar[R, E](ks: K*)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
      ref.updateZIO { cache =>
        ZIO.foreachPar(ks) { k => _get(k).map((k, _)) }.map { cache ++ _ }
      }

    override def invalidate(k: K): UIO[Unit] =
      ref.update(_.removed(k))

    override def invalidateAll(ks: K*): UIO[Unit] =
      ref.update(_.removedAll(ks))

    override def invalidateAll: UIO[Unit] =
      ref.set(Map.empty)

    override def invalidateValues(f: V => Boolean): UIO[Unit] =
      ref.update(_.filter { case (_, v) => !f(v) })

    override def invalidateExpired: UIO[Int] =
      ZIO.succeed(0)

    override def refreshAll[R, E](_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
      ref.updateZIO { cache =>
        ZIO.foreach(cache.keys) { k => _get(k).map((k, _)) }.map(_.toMap)
      }

    override def refreshAllPar[R, E](_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
      ref.updateZIO { cache =>
        ZIO.foreachPar(cache.keys) { k => _get(k).map((k, _)) }.map(_.toMap)
      }

    override def dumpState(logLevel: Logger.LogLevel, showKey: K => String, showValue: V => String): UIO[Unit] =
      for {
        cache <- ref.get
        cacheData = cache.map { case (k, v) =>
          s"\n  - ${showKey(k)} : ${showValue(v)}"
        }
        _ <- Logger.log(logLevel)(s"$toString${cacheData.mkString}")
      } yield ()

    override def toString: String =
      s"Cache[${keyTag.prefixObject}, ${valueTag.prefixObject}](name = $name)"

  }

  private final class WithExpiration[K, V](
      ref: Ref.Synchronized[Map[K, (Instant, V)]],
      expireDuration: Duration,
      name: String,
      keyTag: HTag[K],
      valueTag: HTag[V],
  ) extends Cache[K, V] { // TODO (KR) : consider some of the read functions removing expired value(s)

    private inline def makeExpiration: UIO[Instant] = Clock.instant.map(_.plus(expireDuration))

    override def isCached(k: K): UIO[Boolean] =
      ref.get.map(_.contains(k))

    override def check(k: K, logLevel: Logger.LogLevel): UIO[Option[V]] =
      for {
        now <- Clock.instant
        cache <- ref.get
        res <- cache.get(k) match {
          case Some((vExp, v)) if vExp > now => Logger.log(logLevel)("cache:check", "cache-name" -> name, "cache-result" -> "hit").as(v.some)
          case Some(_)                       => Logger.log(logLevel)("cache:check", "cache-name" -> name, "cache-result" -> "expired").as(None)
          case _                             => Logger.log(logLevel)("cache:check", "cache-name" -> name, "cache-result" -> "miss").as(None)
        }
      } yield res

    override def getOrFetch[R, E](k: K, logLevel: Logger.LogLevel)(_get: K => ZIO[R, E, V]): ZIO[R, E, V] =
      Clock.instant.flatMap { now =>
        ref.modifyZIO { cache =>
          inline def update = for {
            v <- _get(k)
            expAt <- makeExpiration
          } yield (v, cache.updated(k, (expAt, v)))

          cache.get(k) match {
            case Some((vExp, v)) if vExp > now => Logger.log(logLevel)("cache:get-or-fetch", "cache-name" -> name, "cache-result" -> "hit").as((v, cache))
            case Some(_)                       => Logger.log(logLevel)("cache:get-or-fetch", "cache-name" -> name, "cache-result" -> "expired") *> update
            case None                          => Logger.log(logLevel)("cache:get-or-fetch", "cache-name" -> name, "cache-result" -> "miss") *> update
          }
        }
      }

    override def put(k: K, v: V): UIO[Unit] =
      makeExpiration.flatMap { expAt =>
        ref.update(_.updated(k, (expAt, v)))
      }

    override def putAll(pairs: (K, V)*): UIO[Unit] =
      makeExpiration.flatMap { expAt =>
        ref.update(_ ++ pairs.map { case (k, v) => (k, (expAt, v)) })
      }

    override def cache[R, E](k: K)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
      ref.updateZIO { cache =>
        for {
          v <- _get(k)
          expAt <- makeExpiration
        } yield cache.updated(k, (expAt, v))
      }

    override def cacheAll[R, E](ks: K*)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
      ref.updateZIO { cache =>
        for {
          pairs <- ZIO.foreach(ks) { k => _get(k).map((k, _)) }
          expAt <- makeExpiration
        } yield cache ++ pairs.map { case (k, v) => (k, (expAt, v)) }
      }

    override def cacheAllPar[R, E](ks: K*)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
      ref.updateZIO { cache =>
        for {
          pairs <- ZIO.foreachPar(ks) { k => _get(k).map((k, _)) }
          expAt <- makeExpiration
        } yield cache ++ pairs.map { case (k, v) => (k, (expAt, v)) }
      }

    override def invalidate(k: K): UIO[Unit] =
      ref.update(_.removed(k))

    override def invalidateAll(ks: K*): UIO[Unit] =
      ref.update(_.removedAll(ks))

    override def invalidateAll: UIO[Unit] =
      ref.set(Map.empty)

    override def invalidateValues(f: V => Boolean): UIO[Unit] =
      ref.update(_.filter { case (_, (_, v)) => !f(v) })

    override def invalidateExpired: UIO[Int] =
      Clock.instant.flatMap { now =>
        ref.modify { cache =>
          val (valid, invalid) = cache.partition { case (_, (vExp, _)) => vExp > now }
          (invalid.size, valid)
        }
      }

    override def refreshAll[R, E](_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
      ref.updateZIO { cache =>
        for {
          pairs <- ZIO.foreach(cache.keys) { k => _get(k).map((k, _)) }
          expAt <- makeExpiration
        } yield cache ++ pairs.map { case (k, v) => (k, (expAt, v)) }
      }

    override def refreshAllPar[R, E](_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
      ref.updateZIO { cache =>
        for {
          pairs <- ZIO.foreachPar(cache.keys) { k => _get(k).map((k, _)) }
          expAt <- makeExpiration
        } yield cache ++ pairs.map { case (k, v) => (k, (expAt, v)) }
      }

    override def dumpState(logLevel: Logger.LogLevel, showKey: K => String, showValue: V => String): UIO[Unit] =
      for {
        now <- Clock.instant
        cache <- ref.get
        cacheData = cache.map { case (k, (vExp, v)) =>
          val expAtStr = s", exp-at=$vExp"
          val prefix = if (vExp > now) s"  [VALID$expAtStr]" else s"[INVALID$expAtStr]"
          s"\n  - $prefix ${showKey(k)} : ${showValue(v)}"
        }
        _ <- Logger.log(logLevel)(s"$toString [now=$now]${cacheData.mkString}")
      } yield ()

    override def toString: String =
      s"Cache[${keyTag.prefixObject}, ${valueTag.prefixObject}](name = $name, expireDuration = ${expireDuration.render})"

  }

}
