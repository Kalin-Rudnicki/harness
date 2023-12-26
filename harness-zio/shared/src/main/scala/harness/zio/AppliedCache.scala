package harness.zio

import java.time.Instant
import zio.*

final case class AppliedCache[R, E, K, V](_cache: Cache[K, V], _get: K => ZIO[R, E, V]) {

  inline def isCached(k: K): UIO[Boolean] = _cache.isCached(k)
  inline def check(k: K): UIO[Option[V]] = _cache.check(k)

  inline def get(k: K): ZIO[R, E, V] = _cache.get(k)(_get)
  inline def apply(k: K): ZIO[R, E, V] = _cache(k)(_get)

  inline def put(k: K, v: V): UIO[Unit] = _cache.put(k, v)
  inline def putAll(pairs: (K, V)*): UIO[Unit] = _cache.putAll(pairs*)

  inline def cache(k: K): ZIO[R, E, Unit] = _cache.cache(k)(_get)
  inline def cacheAll(ks: K*): ZIO[R, E, Unit] = _cache.cacheAll(ks*)(_get)
  inline def cacheAllPar(ks: K*): ZIO[R, E, Unit] = _cache.cacheAllPar(ks*)(_get)

  inline def invalidate(k: K): UIO[Unit] = _cache.invalidate(k)
  inline def invalidateAll(ks: K*): UIO[Unit] = _cache.invalidateAll(ks*)
  inline def invalidateAll: UIO[Unit] = _cache.invalidateAll

  inline def refreshAll: ZIO[R, E, Unit] = _cache.refreshAll(_get)
  inline def refreshAllPar: ZIO[R, E, Unit] = _cache.refreshAllPar(_get)

}
object AppliedCache {

  def make[R, E, K: Tag, V: Tag](name: String, expireDuration: Option[Duration], get: K => ZIO[R, E, V]): UIO[AppliedCache[R, E, K, V]] =
    Cache.make[K, V](name, expireDuration).map(AppliedCache(_, get))

}
