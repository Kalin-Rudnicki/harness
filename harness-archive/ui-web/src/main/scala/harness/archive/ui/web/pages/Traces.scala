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

object Traces {

  private val timezone = ZoneId.systemDefault()

  final case class Env(
      user: D.user.User,
      query: String,
      apps: Map[D.app.AppId, D.app.App],
      traces: Chunk[Env.Trace],
  )

  object Env {

    final case class Trace(
        app: D.app.App,
        zonedDateTime: ZonedDateTime,
        contexts: List[(String, String)],
        trace: D.telemetry.Trace,
    )
    object Trace {

      def apply(apps: Map[D.app.AppId, D.app.App], trace: D.telemetry.Trace): Env.Trace =
        Env.Trace(
          apps(trace.appId),
          trace.startDateTime.atZoneSameInstant(timezone),
          (trace.logContext ++ trace.telemetryContext).toList.sortBy(_._1),
          trace,
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
        .labelRequired("Traces Query:", "traces-query", inputModifier = width := "100%")
        .zoomOut[Env](_.query)
        .flatMapActionVZM { case (_, query) =>
          Api.telemetry.get(query).map { traces =>
            List(
              Raise.History.PushWithoutNavigation(Url("page", "traces")("query" -> query)),
              Raise.updateState[Env](_.copy(traces = traces.map(Env.Trace(env.apps, _)))),
            )
          }
        }
        .unit
    }

  private val showDate: DateTimeFormatter = DateTimeFormatter.ofPattern("MM/dd/yyyy")
  private val showTime: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm:ss.SSS")

  private def traceRows(rh: RaiseHandler[Nothing, Env], env: Env, trace: Env.Trace): CModifier = {
    def appendQuery(q: String): Unit = {
      val newQuery = s"${env.query} & $q"
      rh.pushUrlWithoutNavigation(Url("page", "traces")("query" -> newQuery))
      rh.raiseZIO {
        Api.telemetry.get(newQuery).map { logs =>
          Raise.updateState[Env](_.copy(query = newQuery, traces = logs.map(Env.Trace(env.apps, _))))
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
        td(textAlign := "center", trace.app.name),
        td(textAlign := "center", trace.zonedDateTime.format(showDate)),
        td(textAlign := "center", trace.zonedDateTime.format(showTime)),
        td(textAlign := "center", (trace.trace.endEpochMS - trace.trace.startEpochMS).toInt.milliseconds.prettyPrint),
        td(
          textAlign := "center",
          colorToCssColor(trace.trace.logLevel.extendedColor),
          color.black,
          trace.trace.logLevel.name,
        ),
        td(
          whiteSpace.preWrap,
          cursor.pointer,
          onClick := { _ => appendQuery(s"message = ${trace.trace.label.unesc}") },
          colSpan := 2,
          trace.trace.label,
        ),
      ),
      trace.contexts match {
        case (cHeadKey, cHeadValue) :: tail =>
          val myRowSpan = trace.contexts.size
          PModifier(
            tr(
              td(rowSpan := myRowSpan, colSpan := 5),
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
            case Some(query) => Api.telemetry.get(query)
            case None        => ZIO.succeed(Chunk.empty)
          }
          // TODO (KR) : get query from URL?
        } yield Env(user, query.getOrElse(""), appMap, logs.map(Env.Trace(appMap, _)))
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
                  th("Duration", rowSpan := 2, width := "200px"),
                  th("Log Level", rowSpan := 2, width := "150px"),
                  th("Message", colSpan := 2, width := "800px"),
                ),
                tr(
                  th("Context Key", width := "250px"),
                  th("Context Value", width := "550px"),
                ),
                PModifier.foreach(env.traces)(traceRows(rh, env, _)),
              )
            },
          ),
        )
      }
      .logA

}
