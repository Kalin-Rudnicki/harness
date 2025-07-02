package harness.webUI

import harness.core.*
import harness.web.*
import harness.webUI.error.UIError
import harness.webUI.facades.ConfigGlobals
import harness.webUI.rawVDOM.Renderer
import harness.webUI.style.{CssClassMap, StyleSheet}
import harness.webUI.vdom.{*, given}
import harness.zio.*
import harness.zio.ErrorLogger.ThrowableInstances.*
import harness.zio.error.ConfigError
import org.scalajs.dom.{console, document, window}
import scala.annotation.tailrec
import zio.*
import zio.json.*

trait PageApp[EnvFromServer <: HasStdClientConfig: {Tag, JsonDecoder}] extends ZIOAppDefault {

  protected val preload: RIO[EnvFromServer, Unit] = ZIO.unit

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

  override final def run: UIO[Unit] =
    (for {
      cfg <- readConfig
      _ <- Logger.withLogger(loggerFromConfig(cfg)).set
      _ <- loadUI.provideLayer(ZLayer.succeed(cfg))
    } yield ()).logErrorDiscard.simpleCause(Logger.LogLevel.Error).unit

  // =====| Helpers |=====

  private def readConfig: IO[ConfigError, EnvFromServer] =
    for {
      cfgStr <- ZIO.attempt { ConfigGlobals.harnessUiConfig }.mapError(ConfigError.LoadError.Generic(ConfigError.ConfigTarget.RawString, _))
      cfg <- ZIO.fromEither(cfgStr.fromJson[EnvFromServer]).mapError(ConfigError.ReadError.DecodingFailure(Nil, _))
    } yield cfg

  private val loggerTarget: Logger.Target =
    event => ZIO.succeed { console.log(event.formatted(ColorMode.Extended, true, true, true)) }

  private val loggerSource: Logger.Source =
    Logger.Source.const("console", loggerTarget, None)

  private def loggerFromConfig(cfg: EnvFromServer): Logger =
    Logger(
      sources = Chunk(loggerSource),
      context = Chunk.empty,
      defaultMinLogTolerance = cfg.stdClientConfig.logLevel,
      forwardToZio = false,
    )

  @tailrec
  private def urlToPageBuilder(runtime: Runtime[EnvFromServer])(url: Url): Page =
    routeMatcher(url) match {
      case RouteMatcher.Result.Success(page) => page
      case RouteMatcher.Result.Fail(error) =>
        error match {
          case UIError.Redirect(url) => urlToPageBuilder(runtime)(url)
          case fail: UIError.Failure => errorPage(fail)
        }
      case RouteMatcher.Result.NotFound => `404Page`(url)
    }

  private val loadUI: RIO[EnvFromServer, Unit] =
    for {
      _ <- Logger.log.info("Welcome to harness-web-ui!") // TODO (KR) : lower this?
      _ <- preload
      runtime <- ZIO.runtime[EnvFromServer]
      renderer <- Renderer.Initial
      urlToPage = urlToPageBuilder(runtime)(_)
      attemptToLoadPage =
        Url.fromWindowURL.flatMap { url =>
          ZIO
            .succeed(urlToPage(url))
            .flatMap(_.replaceNoTrace(renderer, runtime, urlToPage, url))
            .telemetrize("Load Page", Logger.LogLevel.Debug, "url" -> url.path.mkString("/", "/", ""), "stage" -> "attempt-to-load-page")
            .catchAll { errorPage(_).replaceNoTrace(renderer, runtime, urlToPage, url).mapError { e => new RuntimeException(s"Failed to load error page...\n$e") } }
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
    } yield ()

}
object PageApp {

  private[webUI] def runZIO[R](
      runtime: Runtime[R],
      effect: RIO[R, Any],
  ): Unit =
    Unsafe.unsafe { implicit unsafe =>
      runtime.unsafe.runToFuture {
        effect.logErrorDiscard.cause(Logger.LogLevel.Error)
      }
    }

}
