package harness.webUI

import harness.core.*
import harness.webUI.error.UIError
import org.scalajs.dom.window.localStorage
import zio.*
import zio.json.*

object LocalStorage {

  object get {

    // required

    def apply(key: String): IO[UIError.Failure, String] =
      LocalStorage.get.option(key).someOrFail(UIError.Failure.internalDefect(s"Missing localStorage key '$key'"))

    def encoded[A: StringDecoder](key: String): IO[UIError.Failure, A] =
      LocalStorage.get(key).flatMap { s => UIError.fromEither(StringDecoder[A].decode(s)) }

    def json[A: JsonDecoder](key: String): IO[UIError.Failure, A] =
      LocalStorage.get.encoded[A](key)(using StringDecoder.fromJsonDecoder[A])

    // optional

    object option {

      def apply(key: String): IO[UIError.Failure, Option[String]] =
        UIError.attempt { Option(localStorage.getItem(key)) }

      def encoded[A: StringDecoder](key: String): IO[UIError.Failure, Option[A]] =
        LocalStorage.get.option(key).flatMap { ZIO.foreach(_) { s => UIError.fromEither(StringDecoder[A].decode(s)) } }

      def json[A: JsonDecoder](key: String): IO[UIError.Failure, Option[A]] =
        LocalStorage.get.option.encoded[A](key)(using StringDecoder.fromJsonDecoder[A])

    }

  }

  object set {

    // required

    def apply(key: String, value: String): IO[UIError.Failure, Unit] =
      UIError.attempt { localStorage.setItem(key, value) }

    def encoded[A: StringEncoder](key: String, value: A): IO[UIError.Failure, Unit] =
      LocalStorage.set(key, StringEncoder[A].encode(value))

    def json[A: JsonEncoder](key: String, value: A): IO[UIError.Failure, Unit] =
      LocalStorage.set.encoded(key, value)(using StringEncoder.fromJsonEncoder[A])

    // optional

    /**
      * Behavior:
      *   Some(_) -> set
      *   None    -> remove
      */
    object option {

      def apply(key: String, value: Option[String]): IO[UIError.Failure, Unit] = value match
        case Some(value) => LocalStorage.set(key, value)
        case None        => LocalStorage.remove(key)

      def encoded[A: StringEncoder](key: String, value: Option[A]): IO[UIError.Failure, Unit] = value match
        case Some(value) => LocalStorage.set.encoded(key, value)
        case None        => LocalStorage.remove(key)

      def json[A: JsonEncoder](key: String, value: Option[A]): IO[UIError.Failure, Unit] = value match
        case Some(value) => LocalStorage.set.json(key, value)
        case None        => LocalStorage.remove(key)

    }

  }

  def remove(key: String): IO[UIError.Failure, Unit] =
    UIError.attempt { localStorage.removeItem(key) }

}
