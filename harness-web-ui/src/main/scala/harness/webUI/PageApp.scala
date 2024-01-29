package harness.webUI

import cats.data.NonEmptyList
import harness.core.*
import harness.http.client.HttpClient
import harness.web.*
import harness.webUI.facades.ConfigGlobals
import harness.webUI.rawVDOM.Renderer
import harness.webUI.style.{CssClassMap, StyleSheet}
import harness.webUI.vdom.{given, *}
import harness.zio.*
import org.scalajs.dom.{console, document, window}
import scala.annotation.nowarn
import zio.*
import zio.json.*

trait PageApp[EnvFromServer <: HasStdClientConfig: Tag: JsonDecoder] extends ZIOApp {

  override type Environment = HarnessEnv & HttpClient.ClientT & EnvFromServer

  @nowarn
  override implicit def environmentTag: EnvironmentTag[HarnessEnv & HttpClient.ClientT & EnvFromServer] = EnvironmentTag[HarnessEnv & HttpClient.ClientT & EnvFromServer]

  override def bootstrap: HTaskLayer[HarnessEnv & HttpClient.ClientT & EnvFromServer] = {
    val configLayer: HTaskLayer[HConfig] =
      ZLayer.fromZIO {
        for {
          cfgStr <- ZIO.hAttempt { ConfigGlobals.harnessUiConfig }
          hConfig <- HConfig.fromJsonString(cfgStr)
        } yield hConfig
      }

    val loggerLayer: URLayer[StdClientConfig, Logger] =
      ZLayer.fromZIO {
        ZIO.serviceWith[StdClientConfig] { cfg =>
          val target: Logger.Target =
            new Logger.Target {
              override def log(event: Logger.ExecutedEvent): UIO[Unit] = ZIO.hAttempt { console.log(event.formatted(ColorMode.Extended)) }.orDie
            }

          Logger.default(sources = Logger.Source.const(target, None) :: Nil, defaultMinLogTolerance = cfg.logTolerance)
        }
      }

    configLayer >+> HConfig.readLayer[EnvFromServer]() >+> ZLayer.service[EnvFromServer].project(_.stdClientConfig) >+> {
      loggerLayer ++
        ZLayer.succeed(Telemetry.log) ++
        ZLayer.service[EnvFromServer].project(_.stdClientConfig.runMode) ++
        ZLayer.succeed(HError.UserMessage.IfHidden.default) ++
        FileSystem.liveLayer ++
        HttpClient.defaultLayer
    }
  }

  protected val preload: SHRIO[EnvFromServer, Unit] = ZIO.unit

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
  val styleSheets: List[StyleSheet]

  override def run: URIO[HarnessEnv & HttpClient.ClientT & EnvFromServer & Scope, Any] =
    (for {
      _ <- Logger.log.info("Starting page load")
      _ <- preload
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
            .telemetrize("Load Page", Logger.LogLevel.Debug, "url" -> url.path.mkString("/", "/", ""), "stage" -> "attempt-to-load-page")
        }
      cssClassMap = CssClassMap.mergeAll(styleSheets.map(_.toCssClassMap))
      _ <- ZIO.foreachDiscard(cssClassMap.renderOpt) { renderedCss =>
        (for {
          _ <- Logger.log.detailed(s"Adding global style-sheet:\n$renderedCss")
          element <- ZIO.hAttempt { document.createElement("style") }
          _ <- ZIO.hAttempt { element.innerHTML = renderedCss }
          _ <- ZIO.hAttempt { document.head.append(element) }
        } yield ()).mapError(HError.InternalDefect("Unable to add global style-sheet", _))
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
        effect
          .tapError { error => Logger.log.debug(error.fullInternalMessageWithTrace) }
          .dumpErrorsAndContinue(Logger.LogLevel.Error)
      }
    }

}
