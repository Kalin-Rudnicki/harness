package harness.zio

import scala.reflect.ClassTag
import zio.Tag

extension [A](tag: Tag[A]) {
  def toHTag: HTag[A] = HTag.fromTag(tag)
}
extension [A](tag: ClassTag[A]) {
  def toHTag: HTag[A] = HTag.fromClassTag(tag)
}
extension [A](tag: Class[A]) {
  def toHTag: HTag[A] = HTag.fromClass(tag)
}
