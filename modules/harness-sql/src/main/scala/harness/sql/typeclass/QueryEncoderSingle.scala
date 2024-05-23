package harness.sql.typeclass

import cats.syntax.option.*
import org.postgresql.util.PGobject
import scala.reflect.ClassTag
import zio.json.*

trait QueryEncoderSingle[T] { self =>

  val klass: Option[Class[?]]
  def encodeSingle(t: T): Object

  final def cmap[T2](f: T2 => T): QueryEncoderSingle[T2] =
    new QueryEncoderSingle[T2] {
      override val klass: Option[Class[?]] = self.klass
      override def encodeSingle(t: T2): Object = self.encodeSingle(f(t))
    }

  final def optional: QueryEncoderSingle[Option[T]] =
    new QueryEncoderSingle[Option[T]] {
      override val klass: Option[Class[?]] = self.klass
      override def encodeSingle(t: Option[T]): Object = t.fold(null.asInstanceOf[Object])(self.encodeSingle)
    }

}
object QueryEncoderSingle {

  object builder {

    def withoutClass[T <: Object]: QueryEncoderSingle[T] =
      new QueryEncoderSingle[T] {
        override val klass: Option[Class[?]] = None
        override def encodeSingle(t: T): Object = t
      }

    def withClass[T <: Object](implicit ct: ClassTag[T]): QueryEncoderSingle[T] =
      new QueryEncoderSingle[T] {
        override val klass: Option[Class[?]] = ct.runtimeClass.some
        override def encodeSingle(t: T): Object = t
      }

  }

  implicit def fromCodec[T](implicit codec: QueryCodecSingle[T]): QueryEncoderSingle[T] = codec.encoder

  val json: QueryEncoderSingle[String] =
    builder.withoutClass[PGobject].cmap[String] { json =>
      val obj = new PGobject()
      obj.setType("JSON")
      obj.setValue(json)
      obj
    }
  val jsonb: QueryEncoderSingle[String] =
    builder.withoutClass[PGobject].cmap[String] { jsonb =>
      val obj = new PGobject()
      obj.setType("JSONB")
      obj.setValue(jsonb)
      obj
    }
  def encodedJson[T](implicit codec: JsonEncoder[T]): QueryEncoderSingle[T] =
    json.cmap[T] { _.toJson }
  def encodedJsonb[T](implicit codec: JsonEncoder[T]): QueryEncoderSingle[T] =
    jsonb.cmap[T] { _.toJson }

}
