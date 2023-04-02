package harness.zio

import zio.*

final class Cache[-R, +E, -K, +V] private (ref: Ref.Synchronized[Map[K, V]], _get: K => ZIO[R, E, V]) {

  def get(k: K): ZIO[R, E, V] =
    ref.modifyZIO { cached =>
      cached.get(k) match {
        case Some(v) => ZIO.succeed((v, cached))
        case None    => _get(k).map { v => (v, cached.updated(k, v)) }
      }
    }
  inline def apply(k: K): ZIO[R, E, V] = get(k)

  def cache(k: K): ZIO[R, E, Unit] =
    ref.updateZIO { cached =>
      _get(k).map { v => cached.updated(k, v) }
    }
  def cacheAll(ks: K*): ZIO[R, E, Unit] =
    ref.updateZIO { cached =>
      ZIO.foreach(ks) { k => _get(k).map(k -> _) }.map { pairs => cached ++ pairs.toMap }
    }
  def cacheAllPar(ks: K*): ZIO[R, E, Unit] =
    ref.updateZIO { cached =>
      ZIO.foreachPar(ks) { k => _get(k).map(k -> _) }.map { pairs => cached ++ pairs.toMap }
    }

  def invalidate(k: K): UIO[Unit] = ref.update { _.removed(k) }
  def invalidateAll(ks: K*): UIO[Unit] = ref.update { _.removedAll(ks) }
  def invalidateAll: UIO[Unit] = ref.update { _ => Map.empty[K, V] }

  def refreshAll: ZIO[R, E, Unit] =
    ref.updateZIO { cached =>
      ZIO.foreach(cached.keySet.toList) { k => _get(k).map(k -> _) }.map(_.toMap)
    }
  def refreshAllPar: ZIO[R, E, Unit] =
    ref.updateZIO { cached =>
      ZIO.foreachPar(cached.keySet.toList) { k => _get(k).map(k -> _) }.map(_.toMap)
    }

  def isCached(k: K): UIO[Boolean] =
    ref.get.map(_.contains(k))

}
object Cache {

  def apply[R, E, K, V](get: K => ZIO[R, E, V]): UIO[Cache[R, E, K, V]] =
    Ref.Synchronized.make(Map.empty[K, V]).map(new Cache(_, get))

}
