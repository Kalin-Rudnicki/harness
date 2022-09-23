package harness.web.server

import cats.syntax.option.*
import harness.core.*
import zio.json.*

final case class Cookie private (
    private val _name: String,
    private val _value: String,
    private val _secure: Boolean,
    private val _maxAge: Option[Int],
    private val _path: Option[String],
) { self =>

  def secure(s: Boolean): Cookie = self.copy(_secure = s)
  inline def secure: Cookie = self.secure(true)

  def maxAge(a: Option[Int]): Cookie = self.copy(_maxAge = a)
  inline def maxAge(a: Int): Cookie = self.maxAge(a.some)

  def path(p: Option[String]): Cookie = self.copy(_path = p)
  inline def path(p: String): Cookie = self.path(p.some)
  inline def rootPath: Cookie = self.path("/")

  // TODO (KR) : Make sure things are encoded properly
  def cookieString: String =
    List[Option[String]](
      s"${_name}=${_value}".some,
      Option.when(_secure)("Secure"),
      _maxAge.map { a => s"Max-Age=$a" },
      _path.map { p => s"Path=$p" },
    ).flatten.mkString("; ")

}
object Cookie {

  def apply[V](name: String, value: V)(implicit encoder: StringEncoder[V]): Cookie =
    new Cookie(
      _name = name,
      _value = encoder.encode(value),
      _secure = false,
      _maxAge = None,
      _path = None,
    )

  def json[V](name: String, value: V)(implicit encoder: JsonEncoder[V]): Cookie =
    new Cookie(
      _name = name,
      _value = encoder.encodeJson(value, None).toString,
      _secure = false,
      _maxAge = None,
      _path = None,
    )

  def unset(name: String): Cookie =
    new Cookie(
      _name = name,
      _value = "",
      _secure = false,
      _maxAge = 0.some,
      _path = None,
    )

}
