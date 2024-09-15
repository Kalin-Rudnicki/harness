package harness.serviceTracer

import harness.core.IndentedString
import java.time.Instant
import zio.*

final case class TraceElem(
    closure: TraceClosure,
    params: Map[String, String],
    threadName: String,
    start: Instant,
    end: Instant,
    success: Boolean,
    children: Chunk[TraceElem],
) {

  def toIndentedString: IndentedString =
    IndentedString.section(closure.toString)(
      Option.when(params.nonEmpty) {
        IndentedString.section("params:")(
          params.toSeq.map { case (k, v) => s"$k : $v" },
        )
      },
      s"thread-name: $threadName",
      s"span: $start -> $end  (${Duration.fromInterval(start, end).render})",
      s"success: $success",
      IndentedString.section("children:")(
        (children: Seq[TraceElem]).map(_.toIndentedString),
      ),
    )

}
