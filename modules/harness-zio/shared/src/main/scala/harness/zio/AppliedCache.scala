package harness.zio

import zio.*

final case class AppliedCache[R, E, K, V](_cache: Cache[K, V], _get: K => ZIO[R, E, V]) {

  def isCached(k: K): UIO[Boolean] = _cache.isCached(k)

  def check(k: K, logLevel: Logger.LogLevel = Logger.LogLevel.Debug): UIO[Option[V]] = _cache.check(k, logLevel)

  def getOrFetch(k: K, logLevel: Logger.LogLevel = Logger.LogLevel.Debug): ZIO[R, E, V] = _cache.getOrFetch(k, logLevel)(_get)

  def put(k: K, v: V): UIO[Unit] = _cache.put(k, v)

  def putAll(pairs: (K, V)*): UIO[Unit] = _cache.putAll(pairs*)

  def cache(k: K): ZIO[R, E, Unit] = _cache.cache(k)(_get)

  def cacheAll(ks: K*): ZIO[R, E, Unit] = _cache.cacheAll(ks*)(_get)

  def cacheAllPar(ks: K*): ZIO[R, E, Unit] = _cache.cacheAllPar(ks*)(_get)

  def invalidate(k: K): UIO[Unit] = _cache.invalidate(k)

  def invalidateAll(ks: K*): UIO[Unit] = _cache.invalidateAll(ks*)

  def invalidateAll: UIO[Unit] = _cache.invalidateAll

  def invalidateValues(f: V => Boolean): UIO[Unit] = _cache.invalidateValues(f)

  def invalidateExpired: UIO[Int] = _cache.invalidateExpired

  def refreshAll: ZIO[R, E, Unit] = _cache.refreshAll(_get)

  def refreshAllPar: ZIO[R, E, Unit] = _cache.refreshAllPar(_get)

  def dumpState(
      logLevel: Logger.LogLevel = Logger.LogLevel.Info,
      showKey: K => String = (_: K).toString,
      showValue: V => String = (_: V).toString,
  ): UIO[Unit] = _cache.dumpState(logLevel, showKey, showValue)

}
object AppliedCache {

  // =====|  |=====

  def make[R, E, K: HTag, V: HTag](
      name: String,
      expireDuration: Option[Duration],
      initialValues: (K, V)*,
  )(get: K => ZIO[R, E, V]): UIO[AppliedCache[R, E, K, V]] =
    Cache.make[K, V](name, expireDuration, initialValues*).map(AppliedCache(_, get))

  /**
    * Creates a cache where key-value pairs never expire.
    */
  def make[R, E, K: HTag, V: HTag](
      name: String,
      initialValues: (K, V)*,
  )(get: K => ZIO[R, E, V]): UIO[AppliedCache[R, E, K, V]] =
    Cache.make[K, V](name, initialValues*).map(AppliedCache(_, get))

  /**
    * Creates a cache where key-value pairs expire after the provided duration.
    */
  def make[R, E, K: HTag, V: HTag](
      name: String,
      expireDuration: Duration,
      initialValues: (K, V)*,
  )(get: K => ZIO[R, E, V]): UIO[AppliedCache[R, E, K, V]] =
    Cache.make[K, V](name, expireDuration, initialValues*).map(AppliedCache(_, get))

  // TODO (KR) : support schedule with R type
  /**
    * Creates a cache where key-value pairs expire after the provided duration.
    * On the provided schedule, expired items will be removed (for as long as the scope is open).
    */
  def make[R, E, K: Tag, V: Tag](
      name: String,
      expireDuration: Duration,
      clearSchedule: Schedule[Any, Boolean, Any],
      initialValues: (K, V)*,
  )(get: K => ZIO[R, E, V]): URIO[Scope, AppliedCache[R, E, K, V]] =
    Cache.make[K, V](name, expireDuration, clearSchedule, initialValues*).map(AppliedCache(_, get))

  // =====|  |=====

  def layer[R: EnvironmentTag, E: Tag, K: Tag, V: Tag](
      name: String,
      expireDuration: Option[Duration],
      initialValues: (K, V)*,
  )(get: K => ZIO[R, E, V]): ULayer[AppliedCache[R, E, K, V]] =
    ZLayer { AppliedCache.make[R, E, K, V](name, expireDuration, initialValues*)(get) }

  /**
    * Creates a cache where key-value pairs never expire.
    */
  def layer[R: EnvironmentTag, E: Tag, K: Tag, V: Tag](
      name: String,
      initialValues: (K, V)*,
  )(get: K => ZIO[R, E, V]): ULayer[AppliedCache[R, E, K, V]] =
    ZLayer { AppliedCache.make[R, E, K, V](name, initialValues*)(get) }

  /**
    * Creates a cache where key-value pairs expire after the provided duration.
    */
  def layer[R: EnvironmentTag, E: Tag, K: Tag, V: Tag](
      name: String,
      expireDuration: Duration,
      initialValues: (K, V)*,
  )(get: K => ZIO[R, E, V]): ULayer[AppliedCache[R, E, K, V]] =
    ZLayer { AppliedCache.make[R, E, K, V](name, expireDuration, initialValues*)(get) }

  // TODO (KR) : support schedule with R type
  /**
    * Creates a cache where key-value pairs expire after the provided duration.
    * On the provided schedule, expired items will be removed (for as long as the scope is open).
    */
  def layer[R: EnvironmentTag, E: Tag, K: Tag, V: Tag](
      name: String,
      expireDuration: Duration,
      clearSchedule: Schedule[Any, Boolean, Any],
      initialValues: (K, V)*,
  )(get: K => ZIO[R, E, V]): ULayer[AppliedCache[R, E, K, V]] =
    ZLayer.scoped { AppliedCache.make[R, E, K, V](name, expireDuration, clearSchedule, initialValues*)(get) }

  // =====|  |=====

  object unsafe {

    def make[R, E, K: HTag, V: HTag](
        name: String,
        expireDuration: Option[Duration],
        initialValues: (K, V)*,
    )(get: K => ZIO[R, E, V])(implicit unsafe: Unsafe): AppliedCache[R, E, K, V] =
      AppliedCache(Cache.unsafe.make[K, V](name, expireDuration, initialValues*), get)

    /**
      * Creates a cache where key-value pairs never expire.
      */
    def make[R, E, K: HTag, V: HTag](
        name: String,
        initialValues: (K, V)*,
    )(get: K => ZIO[R, E, V])(implicit unsafe: Unsafe): AppliedCache[R, E, K, V] =
      AppliedCache(Cache.unsafe.make[K, V](name, initialValues*), get)

    /**
      * Creates a cache where key-value pairs expire after the provided duration.
      */
    def make[R, E, K: HTag, V: HTag](
        name: String,
        expireDuration: Duration,
        initialValues: (K, V)*,
    )(get: K => ZIO[R, E, V])(implicit unsafe: Unsafe): AppliedCache[R, E, K, V] =
      AppliedCache(Cache.unsafe.make[K, V](name, expireDuration, initialValues*), get)
    
  }

}
