package harness.zio

import cats.syntax.option.*
import harness.core.*
import izumi.reflect.macrortti.*
import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.util.Try
import zio.Tag
import zio.json.JsonCodec

final case class HTag[A](
    packagePrefix: List[String],
    objectPrefix: List[String],
    typeName: String,
    typeArgs: List[HTag[?]],
    klass: Class[?],
) {

  private def genericsWith(f: HTag[?] => String): String =
    if (typeArgs.isEmpty) ""
    else typeArgs.map(f).mkString("[", ", ", "]")

  def prefixWithGenerics(p: HTag.PackagePrefix): String = p match
    case HTag.PackagePrefix.All    => prefixAll
    case HTag.PackagePrefix.Object => prefixObject
    case HTag.PackagePrefix.None   => prefixNone

  def prefixNoGenerics(p: HTag.PackagePrefix): String = p match
    case HTag.PackagePrefix.All    => prefixAllNoGenerics
    case HTag.PackagePrefix.Object => prefixObjectNoGenerics
    case HTag.PackagePrefix.None   => prefixNoneNoGenerics

  def prefix(generics: Boolean, p: HTag.PackagePrefix): String =
    if (generics) prefixWithGenerics(p)
    else prefixNoGenerics(p)

  def prefixAll: String = prefixAllNoGenerics + genericsWith(_.prefixAll)
  def prefixAll(genericsString: HTag[?] => String): String = prefixAllNoGenerics + genericsWith(genericsString)
  def prefixAllNoGenerics: String = List(packagePrefix, objectPrefix, typeName :: Nil).flatten.mkString(".")

  def prefixObject: String = prefixObjectNoGenerics + genericsWith(_.prefixObject)
  def prefixObject(genericsString: HTag[?] => String): String = prefixObjectNoGenerics + genericsWith(genericsString)
  def prefixObjectNoGenerics: String = List(objectPrefix, typeName :: Nil).flatten.mkString(".")

  def prefixNone: String = prefixNoneNoGenerics + genericsWith(_.prefixNone)
  def prefixNone(genericsString: HTag[?] => String): String = prefixNoneNoGenerics + genericsWith(genericsString)
  def prefixNoneNoGenerics: String = typeName

  private def makePrefixString(parts: List[String]): Option[String] = Option.when(parts.nonEmpty)(parts.mkString("."))
  def optPackagePrefixString: Option[String] = makePrefixString(packagePrefix)
  def optObjectPrefixString: Option[String] = makePrefixString(objectPrefix)
  def optPrefixString: Option[String] = makePrefixString(packagePrefix ::: objectPrefix)

  def packagePrefixString: String = optPackagePrefixString.getOrElse("")
  def objectPrefixString: String = optObjectPrefixString.getOrElse("")
  def prefixString: String = optPrefixString.getOrElse("")

  // TODO (KR) : other variants?
  def canonicalName: String =
    s"${(packagePrefix ::: objectPrefix.map(s => s"$s$$")).map(s => s"$s.").mkString}$typeName"

  def toAbstractReference: LightTypeTagRef.AbstractReference =
    LightTypeTagRef.FullReference(
      symName = LightTypeTagRef.SymName.SymTypeName(canonicalName),
      parameters = typeArgs.map { a => LightTypeTagRef.TypeParam(a.toAbstractReference, LightTypeTagRef.Variance.Invariant) },
      prefix = HTag.makeTagPrefix(packagePrefix.map(s => s"$s.").mkString, objectPrefix.reverse),
    )

  def toLightTypeTag: LightTypeTag =
    LightTypeTag(
      toAbstractReference,
      Map.empty,
      Map.empty,
    )

  def withType[B]: HTag[B] = copy()
  def withTypeAndClassTag[B](implicit ct: ClassTag[B]): HTag[B] = copy(klass = ct.runtimeClass)
  def withTypeAndClass[B](klass: Class[?]): HTag[B] = copy(klass = klass)

  def stripGenerics: HTag[A] = copy(typeArgs = Nil)
  def withGenerics(typeArgs: HTag[?]*): HTag[A] = copy(typeArgs = typeArgs.toList)
  def appendGenerics(typeArgs: HTag[?]*): HTag[A] = copy(typeArgs = this.typeArgs ++ typeArgs)

  @tailrec
  def primaryType(maxDepth: Int): HTag[?] =
    typeArgs.lastOption match // `!=` 0 used intentionally to allow "infinite" depth searching
      case Some(last) if maxDepth != 0 => last.primaryType(maxDepth - 1)
      case _                           => this
  def primaryType: HTag[?] = primaryType(-1)

  def contains(that: HTag[?]): Boolean =
    typeArgs.contains(that) || typeArgs.exists(_.contains(that))

  override def equals(that: Any): Boolean = that.asInstanceOf[Matchable] match
    case that: HTag[?] =>
      this.typeName == that.typeName &&
      this.objectPrefix == that.objectPrefix &&
      this.packagePrefix == that.packagePrefix &&
      this.typeArgs == that.typeArgs
    case _ => false

  override def toString: String = prefixAll

}
object HTag {

  enum PackagePrefix extends Enum[PackagePrefix] { case All, Object, None }
  object PackagePrefix extends Enum.Companion[PackagePrefix]

  implicit val ordering: Ordering[HTag[?]] =
    Ordering
      .fromLessThan[HTag[?]] { (a, b) => b.contains(a) }
      .orElseBy(_.primaryType.typeName.toLowerCase)
      .orElseBy(_.typeName.toLowerCase)
      .orElseBy(_.prefixAll.toLowerCase)

  private val suffixRegex = "^(?:[^:$.]+[:$.]+)*([^:$.]+)$".r
  private val packageRegex = "^(.*[:$.])?([^:$.]+)\\.$".r
  private val objectRegex = "^(.*[:$.])?([^:$.]+)(?:::|\\$)$".r

  @tailrec
  private def parsePrefixes(
      str: String,
      packages: List[String],
      objects: List[String],
  ): (List[String], List[String]) =
    str match {
      case ""                       => (packages, objects)
      case objectRegex(start, obj)  => parsePrefixes(Option(start).getOrElse(""), packages, obj :: objects)
      case packageRegex(start, pkg) => parsePrefixes(Option(start).getOrElse(""), pkg :: packages, objects)
      case _                        => throw new RuntimeException(s"Unable to parse package/object: $str")
    }

  private def makeTagPrefix(packagePrefix: String, rObjectPrefix: List[String]): Option[LightTypeTagRef.AppliedReference] =
    rObjectPrefix match {
      case head :: tail =>
        LightTypeTagRef
          .NameReference(
            ref = LightTypeTagRef.SymName.SymTermName(s"$packagePrefix${tail.reverse.map(s => s"$s$$.").mkString}$head"),
            prefix = makeTagPrefix(packagePrefix, tail),
          )
          .some
      case Nil =>
        None
    }

  def make[A](packagePrefix: String*)(objectPrefix: String*)(typeName: String): HTag[A] =
    HTag(packagePrefix.toList, objectPrefix.toList, typeName, Nil, classOf[Any])

  def apply[A](implicit tag: HTag[A]): HTag[A] = tag

  def fromName[A](name: String): HTag[A] =
    name match {
      case suffixRegex(suffix) =>
        val (packagePrefix, objectPrefix) = parsePrefixes(name.stripSuffix(suffix), Nil, Nil)
        HTag(packagePrefix, objectPrefix, suffix, Nil, classOf[Any])
      case _ =>
        HTag(Nil, Nil, name, Nil, classOf[Any])
    }

  def fromLightTypeTag[A](tag: LightTypeTag): HTag[A] = {
    val reprString = tag.repr.split('[').head
    val (packagePrefix, objectPrefix) = parsePrefixes(reprString.stripSuffix(tag.shortName), Nil, Nil)

    HTag(packagePrefix, objectPrefix, tag.shortName, tag.typeArgs.map(fromLightTypeTag), classOf[Any])
  }

  // TODO (KR) : can this be improved?
  def fromClass[A](klass: Class[A]): HTag[A] = {
    val reprString = klass.getName
    val (packagePrefix, objectPrefix) = parsePrefixes(reprString.stripSuffix(klass.getSimpleName), Nil, Nil)

    HTag(
      packagePrefix = packagePrefix,
      objectPrefix = objectPrefix,
      typeName = klass.getSimpleName,
      typeArgs = klass.getTypeParameters.toList.map { a => HTag(Nil, Nil, a.getName, Nil, classOf[Any]) },
      klass = klass,
    )
  }

  def fromTag[A](tag: Tag[A]): HTag[A] = fromLightTypeTag(tag.tag).withTypeAndClass(tag.closestClass)
  implicit def usingTag[A](implicit tag: Tag[A]): HTag[A] = fromTag(tag)

  def fromClassTag[A](tag: ClassTag[A]): HTag[A] = fromClass(tag.runtimeClass).withType[A]
  def usingClassTag[A](implicit tag: ClassTag[A]): HTag[A] = fromClassTag(tag)

  // =====| Encoded |=====

  private final case class Encoded(
      packagePrefix: List[String],
      objectPrefix: List[String],
      typeName: String,
      typeArgs: List[HTag.Encoded],
      klass: String,
  ) derives JsonCodec {

    def toHTag: HTag[?] =
      HTag(
        packagePrefix = packagePrefix,
        objectPrefix = objectPrefix,
        typeName = typeName,
        typeArgs = typeArgs.map(_.toHTag),
        klass = Try { Class.forName(klass) }.getOrElse(classOf[Any]),
      )

  }
  private object Encoded {

    def fromHTag(hTag: HTag[?]): HTag.Encoded =
      HTag.Encoded(
        packagePrefix = hTag.packagePrefix,
        objectPrefix = hTag.objectPrefix,
        typeName = hTag.typeName,
        typeArgs = hTag.typeArgs.map(HTag.Encoded.fromHTag),
        klass = hTag.klass.getName,
      )

  }

  implicit val jsonCodec: JsonCodec[HTag[?]] =
    JsonCodec[HTag.Encoded].transform(_.toHTag, HTag.Encoded.fromHTag)

}
