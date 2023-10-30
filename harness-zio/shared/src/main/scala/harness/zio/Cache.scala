package harness.zio

import zio.*

final class Cache[K, V] private (ref: Ref.Synchronized[Map[K, V]]) {

  def isCached(k: K): UIO[Boolean] =
    ref.get.map(_.contains(k))
  def check(k: K): UIO[Option[V]] =
    ref.get.map(_.get(k))

  def get[R, E](k: K)(_get: K => ZIO[R, E, V]): ZIO[R, E, V] =
    ref.modifyZIO { cached =>
      cached.get(k) match {
        case Some(v) => ZIO.succeed((v, cached))
        case None    => _get(k).map { v => (v, cached.updated(k, v)) }
      }
    }
  inline def apply[R, E](k: K)(_get: K => ZIO[R, E, V]): ZIO[R, E, V] = get(k)(_get)

  def put(k: K, v: V): UIO[Unit] =
    ref.update(_.updated(k, v))
  def putAll(pairs: (K, V)*): UIO[Unit] =
    ref.update(_ ++ pairs.toMap)

  def cache[R, E](k: K)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
    ref.updateZIO { cached =>
      _get(k).map { v => cached.updated(k, v) }
    }
  def cacheAll[R, E](ks: K*)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
    ref.updateZIO { cached =>
      ZIO.foreach(ks) { k => _get(k).map(k -> _) }.map { pairs => cached ++ pairs.toMap }
    }
  def cacheAllPar[R, E](ks: K*)(_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
    ref.updateZIO { cached =>
      ZIO.foreachPar(ks) { k => _get(k).map(k -> _) }.map { pairs => cached ++ pairs.toMap }
    }

  def invalidate(k: K): UIO[Unit] = ref.update { _.removed(k) }
  def invalidateAll(ks: K*): UIO[Unit] = ref.update { _.removedAll(ks) }
  def invalidateAll: UIO[Unit] = ref.update { _ => Map.empty[K, V] }
  def invalidateValues(f: V => Boolean): UIO[Unit] = ref.update { _.toList.filterNot { (_, v) => f(v) }.toMap }

  def refreshAll[R, E](_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
    ref.updateZIO { cached =>
      ZIO.foreach(cached.keySet.toList) { k => _get(k).map(k -> _) }.map(_.toMap)
    }
  def refreshAllPar[R, E](_get: K => ZIO[R, E, V]): ZIO[R, E, Unit] =
    ref.updateZIO { cached =>
      ZIO.foreachPar(cached.keySet.toList) { k => _get(k).map(k -> _) }.map(_.toMap)
    }

}
object Cache {

  def make[K, V]: UIO[Cache[K, V]] = Ref.Synchronized.make(Map.empty[K, V]).map(new Cache(_))

}
