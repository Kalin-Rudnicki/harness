package harness.sql.mock

import harness.pk.TableKey
import harness.zio.test.*
import zio.{test as _, *}
import zio.test.*
import zio.test.Assertion.*

object ExampleSpec extends ZioDefaultHarnessSpec {

  private type Ex1Id = Ex1Id.Id
  private object Ex1Id extends TableKey

  private final case class Ex1(
      id: Ex1Id,
      value: String,
  )

  private type Ex2Id = Ex2Id.Id
  private object Ex2Id extends TableKey

  private final case class Ex2(
      id: Ex2Id,
      ex1Id: Ex1Id,
      value: String,
  )

  private final case class State(
      ex1s: State.Ex1Table,
      ex2s: State.Ex2Table,
  )
  private object State {

    final class Ex1Table private (values: Chunk[Ex1]) extends MockTable[Ex1, Ex1Table]("Ex1", values) {
      val PK = primaryKeyIndex(_.id)
    }
    object Ex1Table {
      implicit val builder: MockTable.Builder[Ex1, Ex1Table] = MockTable.Builder[Ex1, Ex1Table](new Ex1Table(_))
    }

    final class Ex2Table private (values: Chunk[Ex2]) extends MockTable[Ex2, Ex2Table]("Ex2", values) {
      val PK = primaryKeyIndex(_.id)
      val Ex1Index = ManyIndex("Ex1Index", _.ex1Id)
    }
    object Ex2Table {
      implicit val builder: MockTable.Builder[Ex2, Ex2Table] = MockTable.Builder[Ex2, Ex2Table](new Ex2Table(_))
    }

  }

  private final case class Storage(state: MockState[Throwable, State]) {

    def insertEx1(ex1: Ex1): Task[Unit] =
      state.focusAndUpdate(_.ex1s)(_ + ex1)

    def findEx1(k: Ex1Id): UIO[Option[Ex1]] =
      state.get.map(_.ex1s.PK.find(k))

    def getEx1(k: Ex1Id): Task[Ex1] =
      state.get.flatMap(_.ex1s.PK.get(k))

    def updateEx1Value(k: Ex1Id, v: String): Task[Unit] =
      state.focusAndUpdate(_.ex1s)(_.PK.updated(k)(_.copy(value = v)))

    def insertEx2(ex2: Ex2): Task[Unit] =
      state.focusAndUpdateW(_.ex2s) { (a, b) => a.ex1s.PK.get(ex2.ex1Id) *> (b + ex2) }

  }
  private object Storage {
    val layer: URLayer[MockState[Throwable, State], Storage] =
      ZLayer.fromFunction { Storage.apply }
  }

  private val innerSpec: Spec[Storage, Any] =
    suite("ExampleSpec")(
      test("insertEx1") {
        for {
          storage <- ZIO.service[Storage]
          id1 <- Ex1Id.genZio
          id2 <- Ex1Id.genZio
          _ <- storage.insertEx1(Ex1(id1, "abc"))
          _ <- storage.insertEx1(Ex1(id2, "def"))
          res <- storage.insertEx1(Ex1(id1, "ghi")).exit
        } yield assert(res)(fails(anything))
      },
      test("findEx1") {
        for {
          storage <- ZIO.service[Storage]
          id1 <- Ex1Id.genZio
          id2 <- Ex1Id.genZio
          ex = Ex1(id1, "abc")
          _ <- storage.insertEx1(ex)
          res1 <- storage.findEx1(id1)
          res2 <- storage.findEx1(id2)
        } yield assert(res1)(isSome(equalTo(ex))) &&
          assert(res2)(isNone)
      },
      test("getEx1") {
        for {
          storage <- ZIO.service[Storage]
          id1 <- Ex1Id.genZio
          id2 <- Ex1Id.genZio
          ex = Ex1(id1, "abc")
          _ <- storage.insertEx1(ex)
          res1 <- storage.getEx1(id1)
          res2 <- storage.getEx1(id2).exit
        } yield assert(res1)(equalTo(ex)) &&
          assert(res2)(fails(anything))
      },
      test("updateEx1Value") {
        for {
          storage <- ZIO.service[Storage]
          id1 <- Ex1Id.genZio
          id2 <- Ex1Id.genZio
          id3 <- Ex1Id.genZio
          ex1 = Ex1(id1, "abc")
          ex2 = Ex1(id2, "def")
          _ <- storage.insertEx1(ex1)
          _ <- storage.insertEx1(ex2)
          _ <- storage.updateEx1Value(id1, "ghi")
          res1 <- storage.getEx1(id1)
          res2 <- storage.getEx1(id2)
          res3 <- storage.updateEx1Value(id3, "jkl").exit
        } yield assert(res1)(equalTo(ex1.copy(value = "ghi"))) &&
          assert(res2)(equalTo(ex2)) &&
          assert(res3)(fails(anything))
      },
      test("insertEx2") {
        for {
          storage <- ZIO.service[Storage]
          id1 <- Ex1Id.genZio
          id1_1 <- Ex2Id.genZio
          id1_2 <- Ex2Id.genZio
          id2 <- Ex1Id.genZio
          id2_1 <- Ex2Id.genZio
          _ <- storage.insertEx1(Ex1(id1, "abc"))
          _ <- storage.insertEx2(Ex2(id1_1, id1, "abc"))
          _ <- storage.insertEx2(Ex2(id1_2, id1, "def"))
          res1 <- storage.insertEx2(Ex2(id1_2, id1, "ghi")).exit
          res2 <- storage.insertEx2(Ex2(id2_1, id2, "jkl")).exit
        } yield assert(res1)(fails(anything)) &&
          assert(res2)(fails(anything))
      },
    )

  override def spec: TestSpec =
    innerSpec.provideLayer {
      MockState.layer[Throwable, State](
        State(
          MockTable.empty,
          MockTable.empty,
        ),
      ) >>> Storage.layer
    }

}
