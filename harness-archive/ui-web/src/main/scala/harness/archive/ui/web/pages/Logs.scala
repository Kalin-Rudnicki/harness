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
        .flatMapActionVZ { case (_, appName) =>
          Api.log.getForApp(appName).map { logs =>
            Raise.updateState[Env](_.copy(logs = logs.sortBy(_.dateTime).reverse.map(Env.Log(env.apps, _))))
          }
        }
        .unit
    }

  private val showDate: DateTimeFormatter = DateTimeFormatter.ofPattern("MM/dd/yyyy")
  private val showTime: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")

  val page: Page =
    Page.builder
      .fetchState {
        for {
          user <- Api.user.fromSessionTokenOrRedirectToLogin
          apps <- Api.app.getAll
          // TODO (KR) : get query from URL?
        } yield Env(user, "", apps.map(a => a.id -> a).toMap, Chunk.empty)
      }
      .constTitle("Logs")
      .body {
        PModifier(
          Widgets.signedInNavBar.zoomOut[Env](_.user).zoomOutToPage,
          PageWidgets.pageBody(
            h1("Logs"),
            searchWidget,
            br,
            PModifier.builder
              .withState[Env] { env =>
                table(
                  DefaultStyleSheet.stdTable,
                  tr(
                    th("App", width := "200px"),
                    th("Date", width := "150px"),
                    th("Time", width := "150px"),
                    th("Log Level", width := "150px"),
                    th("Message", width := "350px"),
                    th("Context Key", width := "200px"),
                    th("Context Value", width := "300px"),
                  ),
                  PModifier.foreach(env.logs) { log =>
                    val myRowSpan = log.contexts.size.max(1)

                    PModifier(
                      tr(
                        td(rowSpan := myRowSpan, textAlign := "center", log.app.name),
                        td(rowSpan := myRowSpan, textAlign := "center", log.zonedDateTime.format(showDate)),
                        td(rowSpan := myRowSpan, textAlign := "center", log.zonedDateTime.format(showTime)),
                        td(
                          rowSpan := myRowSpan,
                          textAlign := "center",
                          PModifier.foreach(log.log.logLevel) { c =>
                            PModifier(
                              colorToCssColor(c.extendedColor),
                              color.black,
                              c.name,
                            )
                          },
                        ),
                        td(rowSpan := myRowSpan, whiteSpace.preWrap, width := "350px", log.log.message),
                        log.contexts.headOption match {
                          case Some((key, value)) =>
                            PModifier(
                              td(key),
                              td(value),
                            )
                          case None =>
                            td(colSpan := 2)
                        },
                      ),
                      PModifier.foreach(log.contexts.drop(1)) { case (key, value) =>
                        tr(
                          td(key),
                          td(value),
                        )
                      },
                    )
                  },
                )
              },
          ),
        )
      }
      .logA

}
