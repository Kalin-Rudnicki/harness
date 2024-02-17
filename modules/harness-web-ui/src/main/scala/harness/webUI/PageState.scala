package harness.webUI

final case class PageState[+S](
    pageMessages: List[PageMessage],
    state: S,
)
object PageState {
  def init[S](s: S): PageState[S] = PageState(Nil, s)
}
