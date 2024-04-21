package harness.webUI.error

import cats.data.NonEmptyList
import cats.syntax.option.*
import harness.webUI.*
import harness.zio.*
import harness.zio.json.throwableJsonCodec
import zio.json.*

sealed trait UIError
object UIError {

  final case class Redirect(url: Url) extends UIError

  final case class Failure(messages: NonEmptyList[Failure.Message]) extends UIError
  object Failure {

    final case class Message(pageMessage: PageMessage, cause: Option[Throwable])

    def info(message: String): Failure = Failure(NonEmptyList.one(Message(PageMessage.info(message), None)))
    def warning(message: String): Failure = Failure(NonEmptyList.one(Message(PageMessage.warning(message), None)))
    def error(message: String): Failure = Failure(NonEmptyList.one(Message(PageMessage.error(message), None)))

    def infos(messages: NonEmptyList[String]): Failure = Failure(messages.map(m => Message(PageMessage.info(m), None)))
    def warnings(messages: NonEmptyList[String]): Failure = Failure(messages.map(m => Message(PageMessage.warning(m), None)))
    def errors(messages: NonEmptyList[String]): Failure = Failure(messages.map(m => Message(PageMessage.error(m), None)))

    def internalDefect: Failure = Failure.error("An internal defect occurred")
    def internalDefect(causeMessage: String): Failure = Failure.internalDefect(new RuntimeException(causeMessage))
    def internalDefect(cause: Throwable): Failure = Failure(NonEmptyList.one(Message(PageMessage.error("An internal defect occurred"), cause.some)))

    implicit val throwableMapper: ErrorMapper[Throwable, UIError.Failure] =
      UIError.Failure.internalDefect(_)

    implicit val errorLogger: ErrorLogger[UIError.Failure] =
      ErrorLogger
        .withShow[UIError.Failure] {
          _.messages.toList
            .map { msg =>
              s"${msg.pageMessage.title}${msg.cause.fold("")(_.toJsonPretty)}"
            }
            .mkString("\n\n")
        }
        .withLevel(_.messages.head.pageMessage.logLevel) // TODO (KR) : move `logLevel`?

  }

}
