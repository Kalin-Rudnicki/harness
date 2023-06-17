package harness.webUI.style

import harness.webUI.rawVDOM.VDom.{CSSAttr, ScopedName}

final class CssClassMap private (
    private val keySet: Set[String],
    val map: Map[String, CssClassMap.AttrMap],
) { self =>

  private def merge(selfValues: List[CSSAttr], otherValues: List[CSSAttr]): List[CSSAttr] =
    (
      selfValues.map(a => a.scopedName -> a.value).toMap ++
        otherValues.map(a => a.scopedName -> a.value).toMap
    ).toList.map(CSSAttr.apply)

  def ++(other: CssClassMap): CssClassMap = {
    val newKeySet: Set[String] = self.keySet | other.keySet

    if ((self.keySet & other.keySet).isEmpty)
      CssClassMap(newKeySet, self.map ++ other.map)
    else
      CssClassMap(
        newKeySet,
        newKeySet.toList.map { key =>
          (self.map.get(key), other.map.get(key)) match {
            case (Some(selfValues), Some(otherValues)) => (key, selfValues ++ otherValues)
            case (Some(selfValues), None)              => (key, selfValues)
            case (None, Some(otherValues))             => (key, otherValues)
            case (None, None)                          => throw new RuntimeException("key not in either map (impossible)") // shouldn't be possible
          }
        }.toMap,
      )
  }

  def renderOpt: Option[String] = {
    val filteredList = self.map.toList.filter(_._2.list.nonEmpty)

    Option.when(filteredList.nonEmpty) {
      filteredList
        .sortBy(_._1)
        .map { case (name, attrs) =>
          s"$name {${attrs.list.sortBy(_.scopedName.toString).map { attr => s"\n  ${attr.scopedName}: ${attr.value};" }.mkString}\n}"
        }
        .mkString("\n")
    }
  }

  override def toString: String = self.map.toString

}
object CssClassMap {

  final class AttrMap private (
      private val map: Map[ScopedName, String],
      val list: List[CSSAttr],
  ) { self =>

    def ++(other: AttrMap): AttrMap = {
      val map: Map[ScopedName, String] = self.map ++ other.map
      AttrMap(map, map.toList.map(CSSAttr.apply))
    }

    override def toString: String = self.map.toString

  }
  object AttrMap {

    def fromList(list: List[CSSAttr]): AttrMap = {
      val map: Map[ScopedName, String] = list.map(a => a.scopedName -> a.value).toMap
      AttrMap(map, list)
    }

  }

  val empty: CssClassMap = CssClassMap(Set.empty, Map.empty)

  def single(key: String, values: List[CSSAttr]): CssClassMap = CssClassMap(Set(key), Map(key -> AttrMap.fromList(values)))

  def mergeAll(maps: List[CssClassMap]): CssClassMap =
    maps.foldLeft(CssClassMap.empty)(_ ++ _)

}
