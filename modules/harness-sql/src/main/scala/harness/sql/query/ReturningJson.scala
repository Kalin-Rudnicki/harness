package harness.sql.query

import cats.syntax.either.*
import harness.sql.*
import zio.Zippable
import zio.json.JsonDecoder
import zio.json.ast.Json

sealed trait ReturningJson[T] {
  def selectStr: String
  def toMulti: ReturningJson.Multi[T]
  def decoder: JsonDecoder[T]
  def map[T2](f: T => T2): ReturningJson[T2]
  def emap[T2](f: T => Either[String, T2]): ReturningJson[T2]
}
object ReturningJson {

  final case class Single[T](
      keyName: String,
      select: String,
      decoder: JsonDecoder[T],
  ) extends ReturningJson[T] { self =>

    override def selectStr: String = s"to_json($select)"

    override def toMulti: Multi[T] = named(keyName)

    override def map[T2](f: T => T2): ReturningJson.Single[T2] = Single[T2](keyName, select, decoder.map(f))

    override def emap[T2](f: T => Either[String, T2]): ReturningJson.Single[T2] = Single[T2](keyName, select, decoder.mapOrFail(f))

    def named(keyName: String): Multi[T] =
      Multi[T](
        _.get(keyName) match {
          case Some(json) => decoder.fromJsonAST(json)
          case None       => s"missing key '$keyName'".asLeft
        },
        (keyName, select) :: Nil,
      )

  }
  object Single {

    given [T]: Conversion[AppliedCol[T], ReturningJson.Single[T]] = c => Single(c.ref.colName, c.ref.toString, c.col.colCodec.decoder.jsonDecoder)

  }

  final case class Multi[T](
      parse: Map[String, Json] => Either[String, T],
      selects: List[(String, String)],
  ) extends ReturningJson[T] { self =>

    def ~[T2](other: ReturningJson[T2])(implicit zip: Zippable[T, T2]): Multi[zip.Out] = {
      val om = other.toMulti
      Multi[zip.Out](
        map =>
          for {
            t1 <- self.parse(map)
            t2 <- om.parse(map)
          } yield zip.zip(t1, t2),
        self.selects ::: om.selects,
      )
    }

    override def selectStr: String = s"json_build_object(${selects.map { (k, v) => s"'$k', $v" }.mkString(", ")})"

    override def toMulti: Multi[T] = self

    override def decoder: JsonDecoder[T] = JsonDecoder[Json.Obj].mapOrFail { obj => parse(obj.fields.toMap) }

    override def map[T2](f: T => T2): ReturningJson[T2] = Multi[T2](self.parse(_).map(f), self.selects)

    override def emap[T2](f: T => Either[String, T2]): ReturningJson[T2] = Multi[T2](self.parse(_).flatMap(f), self.selects)

  }

}

export ReturningJson.Single.given
