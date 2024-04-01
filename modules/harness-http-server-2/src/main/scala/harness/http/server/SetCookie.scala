package harness.http.server

import cats.syntax.option.*
import harness.core.*
import zio.json.*

final case class SetCookie private (
    private val _name: String,
    private val _value: String,
    private val _secure: Boolean,
    private val _maxAge: Option[Int],
    private val _path: Option[String],
    private val _sameSite: Option[SetCookie.SameSite],
) { self =>

  def secure(s: Boolean): SetCookie = self.copy(_secure = s)
  inline def secure: SetCookie = self.secure(true)

  def maxAge(a: Option[Int]): SetCookie = self.copy(_maxAge = a)
  inline def maxAge(a: Int): SetCookie = self.maxAge(a.some)

  def path(p: Option[String]): SetCookie = self.copy(_path = p)
  inline def path(p: String): SetCookie = self.path(p.some)
  inline def rootPath: SetCookie = self.path("/")

  def sameSite(ss: Option[SetCookie.SameSite]): SetCookie = self.copy(_sameSite = ss)
  inline def sameSite(ss: SetCookie.SameSite): SetCookie = self.sameSite(ss.some)

  // TODO (KR) : Make sure things are encoded properly
  def cookieString: String =
    List[Option[String]](
      s"${_name}=${_value}".some,
      Option.when(_secure)("Secure"),
      _maxAge.map { a => s"Max-Age=$a" },
      _path.map { p => s"Path=$p" },
      _sameSite.map { ss => s"SameSite=$ss" },
    ).flatten.mkString("; ")

  def isUnset: Boolean = _maxAge.contains(0) && _value.isEmpty

  def name: String = _name
  def value: String = _value

}
object SetCookie {

  def apply[V](name: String, value: V)(implicit encoder: StringEncoder[V]): SetCookie =
    new SetCookie(
      _name = name,
      _value = encoder.encode(value),
      _secure = false,
      _maxAge = None,
      _path = None,
      _sameSite = None,
    )

  def json[V](name: String, value: V)(implicit encoder: JsonEncoder[V]): SetCookie =
    new SetCookie(
      _name = name,
      _value = encoder.encodeJson(value, None).toString,
      _secure = false,
      _maxAge = None,
      _path = None,
      _sameSite = None,
    )

  def unset(name: String): SetCookie =
    new SetCookie(
      _name = name,
      _value = "",
      _secure = false,
      _maxAge = 0.some,
      _path = None,
      _sameSite = None,
    )

  enum SameSite { case Lax, Strict, None }

}
