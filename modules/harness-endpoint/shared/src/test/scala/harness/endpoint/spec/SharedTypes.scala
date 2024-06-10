package harness.endpoint.spec

import harness.core.EitherN
import java.util.UUID

object SharedTypes {

  val uuid: UUID = UUID.randomUUID

  sealed trait Tier1Ref
  object Tier1Ref {

    final case class Id(id: UUID) extends Tier1Ref
    final case class Name(name: String) extends Tier1Ref

    def apply(tier1Id: UUID): Tier1Ref = Tier1Ref.Id(tier1Id)
    def apply(tier1Name: String): Tier1Ref = Tier1Ref.Name(tier1Name)

    val pathCodec: PathCodec[Tier1Ref] =
      PathCodec.OneOf[Tier1Ref](
        PathCodec.Const("tier-1") / PathCodec.Param[UUID]("tier-1-id").imap(Id(_), _.id),
        PathCodec.Const("tier-1") / PathCodec.Param[String]("tier-1-name").imap(Name(_), _.name),
      ) {
        case id: Id     => EitherN._1(id)
        case name: Name => EitherN._2(name)
      }

    val queryCodec: QueryCodec[Tier1Ref] =
      QueryCodec.OneOf[Tier1Ref](
        QueryCodec.Required[UUID]("tier-1-id").imap(Id(_), _.id),
        QueryCodec.Required[String]("tier-1-name").imap(Name(_), _.name),
      ) {
        case id: Id     => EitherN._1(id)
        case name: Name => EitherN._2(name)
      }

  }

  sealed trait Tier2Ref
  object Tier2Ref {

    final case class Id(id: UUID) extends Tier2Ref
    final case class NameAndParent(parent: Tier1Ref, name: String) extends Tier2Ref

    def apply(tier2Id: UUID): Tier2Ref = Tier2Ref.Id(tier2Id)
    def apply(tier1Id: UUID, tier2Name: String): Tier2Ref = Tier2Ref.NameAndParent(Tier1Ref(tier1Id), tier2Name)
    def apply(tier1Name: String, tier2Name: String): Tier2Ref = Tier2Ref.NameAndParent(Tier1Ref(tier1Name), tier2Name)

    val pathCodec: PathCodec[Tier2Ref] =
      PathCodec.OneOf[Tier2Ref](
        PathCodec.Const("tier-2") / PathCodec.Param[UUID]("tier-2-id").imap(Id(_), _.id),
        (Tier1Ref.pathCodec / PathCodec.Const("tier-2") / PathCodec.Param[String]("tier-2-name")).imap(NameAndParent(_, _), r => (r.parent, r.name)),
      ) {
        case id: Id              => EitherN._1(id)
        case name: NameAndParent => EitherN._2(name)
      }

    val queryCodec: QueryCodec[Tier2Ref] =
      QueryCodec.OneOf[Tier2Ref](
        QueryCodec.Required[UUID]("tier-2-id").imap(Id(_), _.id),
        (Tier1Ref.queryCodec ++ QueryCodec.Required[String]("tier-2-name")).imap(NameAndParent(_, _), r => (r.parent, r.name)),
      ) {
        case id: Id              => EitherN._1(id)
        case name: NameAndParent => EitherN._2(name)
      }

  }

  sealed trait Tier3Ref
  object Tier3Ref {

    final case class Id(id: UUID) extends Tier3Ref
    final case class NameAndParent(parent: Tier2Ref, name: String) extends Tier3Ref

    def apply(tier3Id: UUID): Tier3Ref = Tier3Ref.Id(tier3Id)
    def apply(tier2Id: UUID, tier3Name: String): Tier3Ref = Tier3Ref.NameAndParent(Tier2Ref(tier2Id), tier3Name)
    def apply(tier1Id: UUID, tier2Name: String, tier3Name: String): Tier3Ref = Tier3Ref.NameAndParent(Tier2Ref(tier1Id, tier2Name), tier3Name)
    def apply(tier1Name: String, tier2Name: String, tier3Name: String): Tier3Ref = Tier3Ref.NameAndParent(Tier2Ref(tier1Name, tier2Name), tier3Name)

    val pathCodec: PathCodec[Tier3Ref] =
      PathCodec.OneOf[Tier3Ref](
        PathCodec.Const("tier-3") / PathCodec.Param[UUID]("tier-3-id").imap(Id(_), _.id),
        (Tier2Ref.pathCodec / PathCodec.Const("tier-3") / PathCodec.Param[String]("tier-3-name")).imap(NameAndParent(_, _), r => (r.parent, r.name)),
      ) {
        case id: Id              => EitherN._1(id)
        case name: NameAndParent => EitherN._2(name)
      }

    val queryCodec: QueryCodec[Tier3Ref] =
      QueryCodec.OneOf[Tier3Ref](
        QueryCodec.Required[UUID]("tier-3-id").imap(Id(_), _.id),
        (Tier2Ref.queryCodec ++ QueryCodec.Required[String]("tier-3-name")).imap(NameAndParent(_, _), r => (r.parent, r.name)),
      ) {
        case id: Id              => EitherN._1(id)
        case name: NameAndParent => EitherN._2(name)
      }

  }

}
