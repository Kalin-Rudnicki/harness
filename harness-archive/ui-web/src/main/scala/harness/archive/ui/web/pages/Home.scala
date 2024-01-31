package harness.archive.ui.web.pages

import cats.syntax.either.*
import harness.archive.model as D
import harness.archive.ui.web.helpers.*
import harness.core.HError
import harness.webUI.*
import harness.webUI.style.{given, *}
import harness.webUI.vdom.{given, *}
import harness.webUI.widgets.*
import harness.zio.*
import org.scalajs.dom.window
import zio.*

object Home {

  final case class Env(
      user: D.user.User,
      apps: Chunk[D.app.App],
      createAppName: String,
      createApp: Boolean,
  )

  private val createAppWidget: Modifier[Env] =
    (
      FormWidgets.textInput[String].labelRequired("App Name:", "app-name").zoomOut[Env](_.createAppName) <*>
        FormWidgets.submitButton("Create App")
    ).flatMapActionVZ { (_, appName) =>
      for {
        app <- Api.app.create(appName)
      } yield Raise.updateState[Env] { env => env.copy(createApp = false, createAppName = "", apps = (env.apps :+ app).sortBy(_.name.toLowerCase)) }
    }.unit

  val page: Page =
    Page.builder
      .fetchState {
        for {
          user <- Api.user.fromSessionTokenOrRedirectToLogin
          apps <- Api.app.getAll
        } yield Env(user, apps.sortBy(_.name.toLowerCase), "", false)
      }
      .constTitle("Home")
      .body {
        PModifier(
          Widgets.signedInNavBar.zoomOut[Env](_.user).zoomOutToPage,
          PageWidgets.pageBody(
            h1("Home"),
            PModifier.builder.withState[D.user.User] { u => p(s"Welcome, ${u.firstName}") }.zoomOut[Env](_.user),
            br,
            PModifier.builder.withRaise.withState[Env] { (rh, env) =>
              PModifier(
                h2("Apps"),
                br,
                if (env.createApp) createAppWidget
                else
                  button(
                    DefaultStyleSheet.button.primary,
                    "Create",
                    onClick := { _ => rh.updateState(_.copy(createApp = true)) },
                  ),
                br,
                br,
                table(
                  DefaultStyleSheet.stdTable,
                  tr(
                    th(width := 350.px, "Id"),
                    th(width := 250.px, "Name"),
                    th(width := 200.px, "Actions"),
                  ),
                  PModifier.foreach(env.apps) { app =>
                    tr(
                      td(textAlign.center, app.id.toString),
                      td(textAlign.center, app.name),
                      td(
                        textAlign.center,
                        button(
                          DefaultStyleSheet.button.primary,
                          "Create API Key",
                          onClick := { _ =>
                            rh.raiseZIO {
                              for {
                                tokenName <- Random.nextUUID
                                (_, token) <- Api.app.generateApiToken(app.id, tokenName.toString)
                                _ <- ZIO.fromPromiseJS { window.navigator.clipboard.writeText(token) }.mapError(HError.fromThrowable)
                              } yield Raise.DisplayMessage(PageMessage.info("API key copied to clipboard"))
                            }
                          },
                        ),
                      ),
                    )
                  },
                ),
              )
            },
          ),
        )
      }
      .logA

}
