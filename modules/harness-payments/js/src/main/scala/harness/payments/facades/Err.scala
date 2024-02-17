package harness.payments.facades

import scala.scalajs.js

@js.native
trait Err extends js.Object {
  val code: String
  val message: String
  val `type`: String
}
