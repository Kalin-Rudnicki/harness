package harness.zio

import java.time.Instant
import scala.math.Ordering.Implicits.infixOrderingOps
import zio.*

final class Cache[K, V] private (
    ref: Ref.Synchronized[Map[K, (Option[Instant], V)]],
    name: String,
    showType: String,
    expireDuration: Option[Duration],
) {

  extension (map: Map[K, (Option[Instant], V)]) {
    private def getValid(k: K, now: Instant): Option[V] =
      map.get(k).collect { case (expires, v) if isValid(expires, now) => v }
  }

  extension (instant: Instant) {
    private def toExpiry: Option[Instant] = expireDuration.map(instant.plus(_))
  }

  private def getExpiresAt: UIO[Option[Instant]] =
    Clock.instant.map(_.toExpiry)

  private def isValid(expiresAt: Option[Instant], now: Instant): Boolean =
    expiresAt match {
      case Some(expiresAt) => now < expiresAt
      case None            => true
    }

  /**
    * This will return true even if the value is expired
    */
  def isCached(k: K): UIO[Boolean] =
    ref.get.map(_.contains(k))
  def check(k: K): UIO[Option[V]] =
    Clock.instant.flatMap { now =>
      ref.get.map(_.getValid(k, now))
    }

  def get[R, E](k: K)(_get: K => ZIO[R, E, V]): ZIO[R, E, V] =
    Clock.instant.flatMap { now =>
      ref.modifyZIO { cached =>
        cached.getValid(k, now) match {
          case Some(v) => ZIO.succeed((v, cached))
          case None    => _get(k).map { v => (v, cached.updated(k, (now.toExpiry, v))) }
        }
      }
    }
  inline def apply[R, E](k: K)(_get: K => ZIO[R, E, V]): ZIO[R, E, V] = get(k)(_get)

  def getLogged[R, E](k: K, logLevel: Logger.LogLevel = Logger.LogLevel.Detailed)(_get: K => ZIO[R, E, V]): ZIO[R, E, V] =
    Clock.instant.flatMap { now =>
      ref.modifyZIO { cached =>
        cached.getValid(k, now) match {
          case Some(v) => Logger.log(logLevel)(s"hit cache '$name' for key: $k").as((v, cached))
          case None    => Logger.log(logLevel)(s"missed cache '$name' for key: $k") *> _get(k).map { v => (v, cached.updated(k, (now.toExpiry, v))) }
        }
      }
    }

  def put(k: K, v: V): UIO[Unit] =
    getExpiresAt.flatMap { exp =>
      ref.update(_.updated(k, (exp, v)))
    }
  def putAll(pairs: (K, V)*): UIO[Unit] =
    getExpiresAt.flatMap { exp =>
      ref.update(_ ++ pairs.map { (k, v) => (k, (exp, v)) }.toMap)
    }

  def cache[R, E](k: K)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
    getExpiresAt.flatMap { exp =>
      ref.updateZIO { cached =>
        _get(k).map { v => cached.updated(k, (exp, v)) }
      }
    }
  def cacheAll[R, E](ks: K*)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
    getExpiresAt.flatMap { exp =>
      ref.updateZIO { cached =>
        ZIO.foreach(ks) { k => _get(k).map(k -> _) }.map { pairs => cached ++ pairs.map { (k, v) => (k, (exp, v)) }.toMap }
      }
    }
  def cacheAllPar[R, E](ks: K*)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
    getExpiresAt.flatMap { exp =>
      ref.updateZIO { cached =>
        ZIO.foreachPar(ks) { k => _get(k).map(k -> _) }.map { pairs => cached ++ pairs.map { (k, v) => (k, (exp, v)) }.toMap }
      }
    }

  def invalidate(k: K): UIO[Unit] = ref.update { _.removed(k) }
  def invalidateAll(ks: K*): UIO[Unit] = ref.update { _.removedAll(ks) }
  def invalidateAll: UIO[Unit] = ref.update { _ => Map.empty[K, (Option[Instant], V)] }
  def invalidateValues(f: V => Boolean): UIO[Unit] = ref.update { _.toList.filterNot { case (_, (_, v)) => f(v) }.toMap }
  def invalidateExpired: UIO[Unit] =
    ZIO
      .when(expireDuration.nonEmpty) {
        Clock.instant.flatMap { now =>
          ref.update(_.filter { case (_, (expires, _)) => isValid(expires, now) })
        }
      }
      .unit

  def refreshAll[R, E](_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
    getExpiresAt.flatMap { exp =>
      ref.updateZIO { cached =>
        ZIO.foreach(cached.keySet.toList) { k => _get(k).map(v => k -> (exp, v)) }.map(_.toMap)
      }
    }
  def refreshAllPar[R, E](_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
    getExpiresAt.flatMap { exp =>
      ref.updateZIO { cached =>
        ZIO.foreachPar(cached.keySet.toList) { k => _get(k).map(v => k -> (exp, v)) }.map(_.toMap)
      }
    }

  override def toString: String = s"Cache$showType($name, $expireDuration)"

}
object Cache {

  def make[K: Tag, V: Tag](name: String, expireDuration: Option[Duration]): UIO[Cache[K, V]] =
    Ref.Synchronized
      .make(Map.empty[K, (Option[Instant], V)])
      .map(
        new Cache(_, name, s"[${HTag[K].prefixObject}, ${HTag[V].prefixObject}]", expireDuration),
      )

}
