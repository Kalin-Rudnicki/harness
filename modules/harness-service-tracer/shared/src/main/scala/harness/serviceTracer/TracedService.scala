package harness.serviceTracer

import harness.serviceTracer.TracedService.functionName
import zio.*

trait TracedService {

  /**
    * In your zio service, this represents the parent trait.
    * Use [[TracedService.Auto]] to handle this automatically.
    *
    * trait MyService {
    *   override protected final val serviceName: String = "MyService"
    * }
    */
  protected val serviceName: String

  /**
    * In your zio service, this represents the implementation.
    * Use [[TracedService.Auto]] to handle this automatically.
    *
    * final case class MyServiceLive() extends MyService {
    *   override protected final val serviceImpl: String = "MyServiceLive"
    * }
    */
  protected val serviceImpl: String

  private def makeClosure(implicit trace: Trace): TraceClosure =
    TraceClosure(serviceName, serviceImpl, trace.functionName)

  final def trace: ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
    new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] {
      def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        effect @@ ServiceTracer.trace(makeClosure)
    }
  final def trace(params: (String, String)*): ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
    new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] {
      def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        effect @@ ServiceTracer.trace(makeClosure, params*)
    }

  final def traceWith(withTrace: TraceElem => UIO[Unit]): ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
    new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] {
      def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        effect @@ ServiceTracer.traceWith(makeClosure)(withTrace)
    }
  final def traceWith(params: (String, String)*)(withTrace: TraceElem => UIO[Unit]): ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] =
    new ZIOAspect[Nothing, Any, Nothing, Any, Nothing, Any] {
      def apply[R, E, A](effect: ZIO[R, E, A])(implicit trace: Trace): ZIO[R, E, A] =
        effect @@ ServiceTracer.traceWith(makeClosure, params*)(withTrace)
    }

  final def traceScoped: URIO[Scope, Unit] =
    ServiceTracer.traceScoped(makeClosure)
  final def traceScoped(params: (String, String)*): URIO[Scope, Unit] =
    ServiceTracer.traceScoped(makeClosure, params*)

}
object TracedService {

  extension (trace: Trace) {
    def functionName: String =
      zio.internal.stacktracer.Tracer.instance.unapply(trace) match
        case Some((loc, _, _)) => loc.stripSuffix(".<init>").split('.').last
        case None              => "<unknown>"
  }

  abstract class Auto(implicit trace: Trace) extends TracedService {

    private def getMyClassName: String = getClass.getSimpleName.stripSuffix("$")

    override protected final val serviceName: String =
      trace.functionName

    override protected final val serviceImpl: String =
      getMyClassName

  }

}
