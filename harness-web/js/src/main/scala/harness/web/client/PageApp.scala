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

  override def bootstrap: ZLayer[Any, NonEmptyList[HError], HarnessEnv] = {
    val loggerLayer: ULayer[Logger] = {
      val target: Logger.Target =
        new Logger.Target {
          override def log(string: String): UIO[Unit] = ZIO.hAttempt("Unable to log to js console") { console.log(string) }.orDieH
        }
      ZLayer.succeed(Logger.default(sources = Logger.Source.const(target, None) :: Nil))
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
  val errorPage: NonEmptyList[HError] => Page = { errors =>
    Page.builder
      .constState(())
      .constTitle("Bad URL")
      .body(
        PModifier(
          h1("Error parsing URL"),
          PModifier.foreach(errors.toList) { e =>
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
      urlToPage = { (url: Url) =>
        routeMatcher(url) match {
          case RouteMatcher.Result.Success(page) => page
          case RouteMatcher.Result.Fail(errors)  => errorPage(errors)
          case RouteMatcher.Result.NotFound      => `404Page`(url)
        }
      }
      attemptToLoadPage =
        for {
          url <- Url.fromWindowURL.toErrorNel
          page = urlToPage(url)
          _ <- page.replaceNoTrace(renderer, runtime, urlToPage)
        } yield ()
      _ <- attemptToLoadPage
      _ <-
        ZIO
          .hAttempt("Unable to set window.onpopstate") {
            window.onpopstate = { _ =>
              PageApp.runZIO(
                runtime,
                attemptToLoadPage,
              )
            }
          }
          .toErrorNel
    } yield ()).dumpErrorsAndContinueNel

}
object PageApp {

  private[client] def runZIO(
      runtime: Runtime[HarnessEnv],
      effect: SHTaskN[Any],
  ): Unit =
    Unsafe.unsafe { implicit unsafe =>
      runtime.unsafe.runToFuture {
        effect
          .mapErrorCause {
            case fail @ Cause.Fail(_, _)     => fail
            case Cause.Die(throwable, trace) => Cause.fail(NonEmptyList.one(HError.InternalDefect("died", throwable)), trace)
            case fail                        => Cause.fail(NonEmptyList.one(HError.InternalDefect(s"Failed with other cause: $fail")), fail.trace)
          }
          .dumpErrorsAndContinueNel(Logger.LogLevel.Error)
      }
    }

}
