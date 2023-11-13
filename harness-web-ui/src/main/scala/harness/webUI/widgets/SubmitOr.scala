package harness.webUI.widgets

sealed trait SubmitOr[+A]

type Submit = Submit.type
case object Submit extends SubmitOr[Nothing]

final case class SubmitOrAction[+A](action: A) extends SubmitOr[A]
