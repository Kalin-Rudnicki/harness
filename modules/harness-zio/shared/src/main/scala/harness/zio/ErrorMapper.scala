package harness.zio

trait ErrorMapper[-E1, E2] {
  def mapError(e: E1): E2
}
object ErrorMapper {

  def apply[E1, E2](implicit errorMapper: ErrorMapper[E1, E2]): ErrorMapper[E1, E2] = errorMapper

  implicit def id[E1, E2 >: E1]: ErrorMapper[E1, E2] = identity(_)

}
