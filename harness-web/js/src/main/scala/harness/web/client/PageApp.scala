package harness.web.client

import cats.data.NonEmptyList
import harness.core.*
import harness.web.client.rawVDOM.Renderer
import harness.web.client.vdom.{given, *}
import harness.zio.*
import org.scalajs.dom.{console, window}
import scala.annotation.nowarn
import zio.*

trait PageApp extends ZIOApp {

  override type Environment = HarnessEnv

  @nowarn
  override implicit def environmentTag: EnvironmentTag[HarnessEnv] = EnvironmentTag[HarnessEnv]

  override def bootstrap: HTaskLayer[HarnessEnv] = {
    val loggerLayer: ULayer[Logger] = {
      val target: Logger.Target =
        new Logger.Target {
          override def log(string: String): UIO[Unit] = ZIO.hAttempt { console.log(string) }.orDie
        }
      ZLayer.succeed(Logger.default(sources = Logger.Source.const(target, None, None) :: Nil, defaultMinLogTolerance = logTolerance))
    }

    loggerLayer ++
      ZLayer.succeed(runMode) ++
      ZLayer.succeed(HError.UserMessage.IfHidden.default) ++
      FileSystem.liveLayer
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

  override def run: URIO[HarnessEnv & Scope, Any] =
    (for {
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
        for {
          url <- Url.fromWindowURL
          page = urlToPage(url)
          _ <- page.replaceNoTrace(renderer, runtime, urlToPage)
        } yield ()
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

  private[client] def runZIO(
      runtime: Runtime[HarnessEnv],
      effect: SHTask[Any],
  ): Unit =
    Unsafe.unsafe { implicit unsafe =>
      runtime.unsafe.runToFuture {
        effect.collapseCause
          .dumpErrorsAndContinue(Logger.LogLevel.Error)
      }
    }

}
