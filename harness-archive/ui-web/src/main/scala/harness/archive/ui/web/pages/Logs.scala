package harness.archive.ui.web.pages

import cats.syntax.either.*
import harness.archive.model as D
import harness.archive.ui.web.helpers.*
import harness.core.*
import harness.webUI.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*
import java.time.{ZonedDateTime, ZoneId}
import java.time.format.DateTimeFormatter
import zio.*

object Logs {

  private val timezone = ZoneId.systemDefault()

  final case class Env(
      user: D.user.User,
      query: String,
      apps: Map[D.app.AppId, D.app.App],
      logs: Chunk[Env.Log],
  )
  object Env {

    final case class Log(
        app: D.app.App,
        zonedDateTime: ZonedDateTime,
        contexts: List[(String, String)],
        log: D.log.Log,
    )
    object Log {

      def apply(apps: Map[D.app.AppId, D.app.App], log: D.log.Log): Env.Log =
        Env.Log(
          apps(log.appId),
          log.dateTime.atZoneSameInstant(timezone),
          log.context.toList.sortBy(_._1),
          log,
        )

    }

  }

  private def colorToCssColor(c: Color): CModifier =
    c match {
      case Color.RGB(r, g, b) => backgroundColor := s"rgb($r, $g, $b)"
      case named: Color.Named => backgroundColor := named.toString.toLowerCase
      case Color.Default      => backgroundColor.white
    }

  private val searchWidget: Modifier[Env] =
    PModifier.builder.withState[Env] { env =>
      FormWidgets
        .textInput[String]
        .labelRequired("Logs Query:", "log-query", inputModifier = width := "100%")
        .zoomOut[Env](_.query)
        .flatMapActionVZM { case (_, query) =>
          Api.log.get(query).map { logs =>
            List(
              Raise.History.PushWithoutNavigation(Url("page", "logs")("query" -> query)),
              Raise.updateState[Env](_.copy(logs = logs.sortBy(_.dateTime).reverse.map(Env.Log(env.apps, _)))),
            )
          }
        }
        .unit
    }

  private val showDate: DateTimeFormatter = DateTimeFormatter.ofPattern("MM/dd/yyyy")
  private val showTime: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")

  private def logRows(rh: RaiseHandler[Nothing, Env], env: Env, log: Env.Log): CModifier = {
    def appendQuery(q: String): Unit = {
      val newQuery = s"${env.query} & $q"
      rh.pushUrlWithoutNavigation(Url("page", "logs")("query" -> newQuery))
      rh.raiseZIO {
        Api.log.get(newQuery).map { logs =>
          Raise.updateState[Env](_.copy(query = newQuery, logs = logs.map(Env.Log(env.apps, _))))
        }
      }
    }

    def contextCells(key: String, value: String): CModifier =
      PModifier(
        td(
          key,
          cursor.pointer,
          onClick := { _ => appendQuery(s"${s"context.$key".unesc}?") },
        ),
        td(
          whiteSpace.preWrap,
          cursor.pointer,
          onClick := { _ => appendQuery(s"${s"context.$key".unesc} = ${value.unesc}") },
          value,
        ),
      )

    PModifier(
      tr(
        td(textAlign := "center", log.app.name),
        td(textAlign := "center", log.zonedDateTime.format(showDate)),
        td(textAlign := "center", log.zonedDateTime.format(showTime)),
        td(
          textAlign := "center",
          PModifier.foreach(log.log.logLevel) { c =>
            PModifier(
              colorToCssColor(c.extendedColor),
              color.black,
              c.name,
            )
          },
        ),
        td(whiteSpace.preWrap, colSpan := 2, log.log.message),
      ),
      log.contexts match {
        case (cHeadKey, cHeadValue) :: tail =>
          val myRowSpan = log.contexts.size
          PModifier(
            tr(
              td(rowSpan := myRowSpan, colSpan := 4),
              contextCells(cHeadKey, cHeadValue),
            ),
            PModifier.foreach(tail) { case (key, value) => tr(contextCells(key, value)) },
          )
        case Nil =>
          PModifier()
      },
    )
  }

  def page(query: Option[String]): Page =
    Page.builder
      .fetchState {
        for {
          user <- Api.user.fromSessionTokenOrRedirectToLogin
          apps <- Api.app.getAll
          appMap = apps.map(a => a.id -> a).toMap
          logs <- query match {
            case Some(query) => Api.log.get(query)
            case None        => ZIO.succeed(Chunk.empty)
          }
          // TODO (KR) : get query from URL?
        } yield Env(user, query.getOrElse(""), appMap, logs.map(Env.Log(appMap, _)))
      }
      .constTitle("Logs")
      .body {
        PModifier(
          Widgets.signedInNavBar.zoomOut[Env](_.user).zoomOutToPage,
          PageWidgets.pageBody(
            h1("Logs"),
            searchWidget,
            br,
            PModifier.builder.withRaise.withState[Env] { (rh, env) =>
              table(
                DefaultStyleSheet.stdTable,
                tr(
                  th("App", rowSpan := 2, width := "200px"),
                  th("Date", rowSpan := 2, width := "150px"),
                  th("Time", rowSpan := 2, width := "150px"),
                  th("Log Level", rowSpan := 2, width := "150px"),
                  th("Message", colSpan := 2, width := "1000px"),
                ),
                tr(
                  th("Context Key", width := "250px"),
                  th("Context Value", width := "750px"),
                ),
                PModifier.foreach(env.logs)(logRows(rh, env, _)),
              )
            },
          ),
        )
      }
      .logA

}
