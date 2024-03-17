package harness.webUI

import cats.syntax.option.*
import harness.core.*
import harness.http.client.HttpClient
import harness.web.*
import harness.webUI.error.UIError
import harness.webUI.facades.ConfigGlobals
import harness.webUI.rawVDOM.Renderer
import harness.webUI.style.{CssClassMap, StyleSheet}
import harness.webUI.vdom.{given, *}
import harness.zio.*
import harness.zio.error.ConfigError
import org.scalajs.dom.{console, document, window}
import scala.annotation.{nowarn, tailrec}
import zio.*
import zio.json.*

trait PageApp[EnvFromServer <: HasStdClientConfig: Tag: JsonDecoder] extends ZIOApp {

  override type Environment = HarnessEnv & HttpClient.ClientT & EnvFromServer

  @nowarn
  override implicit def environmentTag: EnvironmentTag[HarnessEnv & HttpClient.ClientT & EnvFromServer] = EnvironmentTag[HarnessEnv & HttpClient.ClientT & EnvFromServer]

  override def bootstrap: TaskLayer[HarnessEnv & HttpClient.ClientT & EnvFromServer] = {
    val configLayer: TaskLayer[HConfig] =
      ZLayer.fromZIO {
        for {
          cfgStr <- ZIO.attempt { ConfigGlobals.harnessUiConfig }
          hConfig <- HConfig.fromJsonString(cfgStr, ConfigError.ConfigTarget.RawString)
        } yield hConfig
      }

    val loggerLayer: URLayer[StdClientConfig, Logger] =
      ZLayer.fromZIO {
        ZIO.serviceWith[StdClientConfig] { cfg =>
          val target: Logger.Target =
            new Logger.Target {
              override def log(event: Logger.ExecutedEvent): UIO[Unit] = ZIO.attempt { console.log(event.formatted(ColorMode.Extended)) }.orDie
            }

          Logger.default(sources = Logger.Source.const(target, None) :: Nil, defaultMinLogTolerance = cfg.logTolerance)
        }
      }

    configLayer >+> HConfig.readLayer[EnvFromServer]() >+> ZLayer.service[EnvFromServer].project(_.stdClientConfig) >+> {
      loggerLayer ++
        ZLayer.succeed(Telemetry.log) ++
        FileSystem.liveLayer ++
        HttpClient.defaultLayer
    }
  }

  protected val preload: RIO[Environment, Unit] = ZIO.unit

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
  val errorPage: UIError.Failure => Page = { error =>
    Page.builder
      .constState(())
      .constTitle("Bad URL")
      .body(
        PModifier(
          h1("Error parsing URL"),
          ul(
            PModifier.foreach(error.messages.toList.map(_.pageMessage.title))(li(_)),
          ),
        ),
      )
      .logA
  }

  val routeMatcher: RouteMatcher.Root
  val styleSheets: List[StyleSheet]

  @tailrec
  private def urlToPageBuilder(runtime: Runtime[HarnessEnv & HttpClient.ClientT & EnvFromServer])(url: Url): Page =
    routeMatcher(url) match {
      case RouteMatcher.Result.Success(page) => page
      case RouteMatcher.Result.Fail(error) =>
        error match {
          case UIError.Redirect(url) => urlToPageBuilder(runtime)(url)
          case fail: UIError.Failure => errorPage(fail)
        }
      case RouteMatcher.Result.NotFound => `404Page`(url)
    }

  override def run: URIO[HarnessEnv & HttpClient.ClientT & EnvFromServer & Scope, Any] =
    (for {
      _ <- Logger.log.info("Starting page load")
      _ <- preload
      runtime <- bootstrap.toRuntime
      renderer <- Renderer.Initial
      urlToPage = urlToPageBuilder(runtime)(_)
      attemptToLoadPage =
        Url.fromWindowURL.flatMap { url =>
          ZIO
            .succeed(urlToPage(url))
            .flatMap(_.replaceNoTrace(renderer, runtime, urlToPage, url))
            .telemetrize("Load Page", Logger.LogLevel.Debug, "url" -> url.path.mkString("/", "/", ""), "stage" -> "attempt-to-load-page")
            .catchAll {
              errorPage(_).replaceNoTrace(renderer, runtime, urlToPage, url).mapError { e => new RuntimeException(s"Failed to load error page...\n$e") }
            }
        }
      cssClassMap = CssClassMap.mergeAll(styleSheets.map(_.toCssClassMap))
      _ <- ZIO.foreachDiscard(cssClassMap.renderOpt) { renderedCss =>
        for {
          _ <- Logger.log.detailed(s"Adding global style-sheet:\n$renderedCss")
          element <- ZIO.attempt { document.createElement("style") }
          _ <- ZIO.attempt { element.innerHTML = renderedCss }
          _ <- ZIO.attempt { document.head.append(element) }
        } yield ()
      }
      _ <- attemptToLoadPage
      _ <-
        ZIO.attempt {
          window.onpopstate = { _ =>
            PageApp.runZIO(
              runtime,
              attemptToLoadPage,
            )
          }
        }
    } yield ()).logErrorCauseSimpleAndContinue(Logger.LogLevel.Error, Logger.LogLevel.Debug.some)(using ErrorLogger.ThrowableInstances.jsonErrorLogger)

}
object PageApp {

  private[webUI] def runZIO(
      runtime: Runtime[HarnessEnv & HttpClient.ClientT],
      effect: RIO[HarnessEnv & HttpClient.ClientT, Any],
  ): Unit =
    Unsafe.unsafe { implicit unsafe =>
      runtime.unsafe.runToFuture {
        effect.logErrorCauseSimpleAndContinue(Logger.LogLevel.Error, Logger.LogLevel.Debug.some)(using ErrorLogger.ThrowableInstances.jsonErrorLogger)
      }
    }

}
