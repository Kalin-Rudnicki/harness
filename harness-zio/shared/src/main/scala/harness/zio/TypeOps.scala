package harness.zio

import harness.core.Enum
import izumi.reflect.Tag

object TypeOps {

  enum PackagePrefix extends Enum[PackagePrefix] { case All, Object, None }
  object PackagePrefix extends Enum.Companion[PackagePrefix]

  private def deGenerify(str: String, generics: Boolean): String =
    if (generics) str else str.split('[').head

  private def cleanse(str: String): String =
    str.replaceAll("\\$::", ".").replaceAll("[+\\-=]", "")

  private def stripPrefixes(str: String): String =
    str.replaceAll("([A-Za-z0-9_]+\\.)+", "")

  def typeName[T](generics: Boolean, packagePrefix: PackagePrefix)(implicit tag: Tag[T]): String =
    (packagePrefix match {
      case PackagePrefix.All    => deGenerify(cleanse(tag.tag.repr), generics)
      case PackagePrefix.Object => deGenerify(cleanse(tag.tag.toString), generics)
      case PackagePrefix.None   => stripPrefixes(deGenerify(cleanse(tag.tag.toString), generics))
    }).replaceAll(",", ", ")
  def typeName[T](implicit tag: Tag[T]): String =
    typeName[T](true, PackagePrefix.Object)

}
