package harness.sql.mock

import harness.sql.mock.error.*
import zio.*

abstract class MockTable[V, T <: MockTable[V, T]](final val tableName: String, final val values: Chunk[V])(implicit builder: MockTable.Builder[V, T]) { table: T =>

  final def updatedWith(filter: V => Boolean)(update: V => V): IO[MockError, T] =
    builder.safeBuild {
      values.map {
        case v if filter(v) => update(v)
        case v              => v
      }
    }

  final def +(v: V): IO[MockError, T] =
    builder.safeBuild { values :+ v }

  final def ++(vs: Iterable[V]): IO[MockError, T] =
    builder.safeBuild { values ++ vs }

  final case class UniqueIndex[K](indexName: String, key: V => K) {

    private val indexMap: Map[K, V] =
      values.groupBy(key).map {
        case (k, Chunk(v)) => (k, v)
        case (k, _)        => throw MockError.ConstraintViolationError(tableName, indexName, k)
      }

    final def find(k: K): Option[V] =
      indexMap.get(k)

    final def get(k: K): IO[MockError.MissingExpectedKeyError, V] =
      ZIO.getOrFailWith(MockError.MissingExpectedKeyError(tableName, k))(find(k))

    final def updated(k: K)(update: V => V): IO[MockError, T] =
      get(k) *> table.updatedWith(key(_) == k)(update)

    final def removed(k: K): IO[MockError, T] =
      get(k) *> builder.safeBuild(table.values.filterNot(key(_) == k))

  }

  def primaryKeyIndex[K](key: V => K): UniqueIndex[K] = UniqueIndex[K]("PrimaryKeyIndex", key)

  final case class ManyIndex[K](indexName: String, key: V => K) {

    private val indexMap: Map[K, Chunk[V]] =
      values.groupBy(key)

    final def find(k: K): Chunk[V] =
      indexMap.getOrElse(k, Chunk.empty)

  }

}
object MockTable {

  final case class Builder[V, T <: MockTable[V, T]](build: Chunk[V] => T) {
    def safeBuild(values: => Chunk[V]): IO[MockError, T] =
      ZIO
        .attempt { build(values) }
        .refineOrDie { case e: MockError => e }
  }

  def empty[V, T <: MockTable[V, T]](implicit builder: Builder[V, T]): T =
    builder.build(Chunk.empty)

  def build[V, T <: MockTable[V, T]](values: Iterable[V])(implicit builder: Builder[V, T]): T =
    builder.build(Chunk.empty)

}
