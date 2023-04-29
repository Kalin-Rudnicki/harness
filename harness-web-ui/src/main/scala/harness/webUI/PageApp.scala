package harness.webUI

import cats.data.NonEmptyList
import harness.core.*
import harness.http.client.HttpClient
import harness.webUI.rawVDOM.Renderer
import harness.webUI.vdom.{given, *}
import harness.zio.*
import org.scalajs.dom.{console, window}
import scala.annotation.nowarn
import zio.*

trait PageApp extends ZIOApp {

  override type Environment = HarnessEnv & HttpClient.ClientT

  @nowarn
  override implicit def environmentTag: EnvironmentTag[HarnessEnv & HttpClient.ClientT] = EnvironmentTag[HarnessEnv & HttpClient.ClientT]

  override def bootstrap: HTaskLayer[HarnessEnv & HttpClient.ClientT] = {
    val loggerLayer: ULayer[Logger] = {
      val target: Logger.Target =
        new Logger.Target {
          override def log(event: Logger.ExecutedEvent): UIO[Unit] = ZIO.hAttempt { console.log(event.formatted) }.orDie
        }
      ZLayer.succeed(Logger.default(sources = Logger.Source.const(target, None, None) :: Nil, defaultMinLogTolerance = logTolerance))
    }

    loggerLayer ++
      ZLayer.succeed(Telemetry.log) ++
      ZLayer.succeed(runMode) ++
      ZLayer.succeed(HError.UserMessage.IfHidden.default) ++
      FileSystem.liveLayer ++
      HttpClient.defaultLayer
  }

  // TODO (KR) : Come up with a better system for this
  protected val runMode: RunMode = RunMode.Prod
  protected val logTolerance: Logger.LogLevel = Logger.LogLevel.Info

  // TODO (KR) : Make this prettier
  /**
    * Page to display on 404-not-found. You can override this in your app.
    */
  val `404Page`: Url => Page = { url =>
    Page.builder
      .constState(())
      .constTitle("404 Not Found")
      .body(
        PModifier(
          h1("404 Not Found"),
          h3(url.toString),
        ),
      )
      .logA
  }
  // TODO (KR) : Make this prettier
  /**
    * Page to display when URL parsing results in an error. You can override this in your app.
    */
  val errorPage: (HError.UserMessage.IfHidden, HError) => Page = { (ifHidden, error) =>
    Page.builder
      .constState(())
      .constTitle("Bad URL")
      .body(
        PModifier(
          h1("Error parsing URL"),
          PModifier.foreach(error.toNel.toList) { e =>
            h3(e.userMessage.show(ifHidden)) // TODO (KR) :
          },
        ),
      )
      .logA
  }

  val routeMatcher: RouteMatcher.Root

  override def run: URIO[HarnessEnv & HttpClient.ClientT & Scope, Any] =
    (for {
      _ <- Logger.log.info("Starting page load")
      runtime <- bootstrap.toRuntime
      renderer <- Renderer.Initial
      urlToPage = { (url: Url) =>
        routeMatcher(url) match {
          case RouteMatcher.Result.Success(page) => page
          case RouteMatcher.Result.Fail(error)   => errorPage(runtime.environment.get[HError.UserMessage.IfHidden], error)
          case RouteMatcher.Result.NotFound      => `404Page`(url)
        }
      }
      attemptToLoadPage =
        Url.fromWindowURL.flatMap { url =>
          ZIO
            .succeed(urlToPage(url))
            .flatMap(_.replaceNoTrace(renderer, runtime, urlToPage, url))
            .trace("Load Page", Logger.LogLevel.Debug, "url" -> url.path.mkString("/", "/", ""), "stage" -> "attempt-to-load-page")
        }
      _ <- attemptToLoadPage
      _ <-
        ZIO.hAttempt {
          window.onpopstate = { _ =>
            PageApp.runZIO(
              runtime,
              attemptToLoadPage,
            )
          }
        }
    } yield ()).dumpErrorsAndContinue

}
object PageApp {

  private[webUI] def runZIO(
      runtime: Runtime[HarnessEnv & HttpClient.ClientT],
      effect: SHRIO[HttpClient.ClientT, Any],
  ): Unit =
    Unsafe.unsafe { implicit unsafe =>
      runtime.unsafe.runToFuture {
        effect.dumpErrorsAndContinue(Logger.LogLevel.Error)
      }
    }

}
