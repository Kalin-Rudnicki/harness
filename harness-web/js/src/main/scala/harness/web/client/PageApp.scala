package harness.web.client

import cats.data.NonEmptyList
import harness.core.*
import harness.web.client.rawVDOM.Renderer
import harness.web.client.vdom.{given, *}
import harness.zio.*
import org.scalajs.dom.console
import scala.annotation.nowarn
import zio.*

trait PageApp extends ZIOApp {

  override type Environment = HarnessEnv

  @nowarn
  override implicit def environmentTag: EnvironmentTag[HarnessEnv] = EnvironmentTag[HarnessEnv]

  override def bootstrap: ZLayer[Any, NonEmptyList[HError], HarnessEnv] = {
    val loggerLayer: ULayer[Logger] = {
      val target: Logger.Target =
        new Logger.Target {
          override def log(string: String): UIO[Unit] = ZIO.hAttempt("Unable to log to js console") { console.log(string) }.orDieH
        }
      ZLayer.succeed(Logger(Logger.Source.const(target, Logger.LogLevel.Info, Logger.LogLevel.Always) :: Nil))
    }

    loggerLayer ++
      ZLayer.succeed(runMode) ++
      FileSystem.liveLayer.toErrorNel
  }

  // TODO (KR) : Come up with a better system for this
  protected val runMode: RunMode = RunMode.Dev

  // TODO (KR) : Make this prettier
  /**
    * Page to display on 404-not-found. You can override this in your app.
    */
  val `404Page`: Url => Page = { url =>
    Page(url)
      .constState(())
      .constTitle("404 Not Found")
      .body(
        PWidget(
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
  val errorPage: NonEmptyList[HError] => Url => Page = { errors => url =>
    Page(url)
      .constState(())
      .constTitle("Bad URL")
      .body(
        PWidget(
          h1("Error parsing URL"),
          PWidget.foreach(errors.toList) { e =>
            h3(e.userMessage)
          },
        ),
      )
      .logA
  }

  val routeMatcher: RouteMatcher.Root

  override def run: ZIO[HarnessEnv & Scope, Nothing, Any] =
    (for {
      runtime <- bootstrap.toRuntime
      renderer <- Renderer.Initial
      url <- Url.fromWindowURL.toErrorNel
      page =
        routeMatcher(url) match {
          case RouteMatcher.Result.Success(page) => page
          case RouteMatcher.Result.Fail(errors)  => errorPage(errors)(url)
          case RouteMatcher.Result.NotFound      => `404Page`(url)
        }
      _ <- page.replaceNoTrace(renderer, runtime)
    } yield ()).dumpErrorsAndContinueNel

}