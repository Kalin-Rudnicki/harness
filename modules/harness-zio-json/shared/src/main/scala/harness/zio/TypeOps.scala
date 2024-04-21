package harness.zio

import harness.core.Enum
import izumi.reflect.Tag
import izumi.reflect.macrortti.*

implicit class TypeOps[A](tag: Tag[A]) {
  import TypeOps.*

  object typeName {

    def apply(generics: Boolean, packagePrefix: PackagePrefix): String =
      (packagePrefix match {
        case PackagePrefix.All    => deGenerify(cleanse(tag.tag.repr), generics)
        case PackagePrefix.Object => deGenerify(cleanse(tag.tag.toString), generics)
        case PackagePrefix.None   => stripPrefixes(deGenerify(cleanse(tag.tag.toString), generics))
      }).replaceAll(",", ", ")

    def prefixAll: String = typeName(true, PackagePrefix.All)
    def prefixAllNoGenerics: String = typeName(false, PackagePrefix.All)

    def prefixObject: String = typeName(true, PackagePrefix.Object)
    def prefixObjectNoGenerics: String = typeName(false, PackagePrefix.Object)

    def prefixNone: String = typeName(true, PackagePrefix.None)
    def prefixNoneNoGenerics: String = typeName(false, PackagePrefix.None)

  }

}
object TypeOps {

  enum PackagePrefix extends Enum[PackagePrefix] { case All, Object, None }
  object PackagePrefix extends Enum.Companion[PackagePrefix]

  private def deGenerify(str: String, generics: Boolean): String =
    if (generics) str else str.split('[').head

  private def cleanse(str: String): String =
    str.replaceAll("\\$?::", ".").replaceAll("[+\\-=]", "")

  private def stripPrefixes(str: String): String =
    str.replaceAll("([A-Za-z0-9_]+\\.)+", "")

  def tagFromClass[A](klass: Class[?]): zio.Tag[A] =
    new zio.Tag[A] {
      override val tag: zio.LightTypeTag = {
        val reg = "^(?:(.+)\\.)?([^.]+)$".r
        val (prefix, name) = klass.getName match {
          case reg(p, n) => Option(p) -> n
          case _         => None -> klass.getSimpleName
        }

        izumi.reflect.macrortti.LightTypeTag(
          LightTypeTagRef.NameReference(
            LightTypeTagRef.SymName.SymTypeName(name.stripSuffix("$")),
            prefix = prefix.map { p => LightTypeTagRef.NameReference(LightTypeTagRef.SymName.SymTermName(p)) },
          ),
          Map.empty,
          Map.empty,
        )
      }

      override def closestClass: Class[?] = klass
    }

}
