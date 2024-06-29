package harness.zio.mock

import harness.zio.*
import harness.zio.mock.*
import harness.zio.test.*
import java.util.UUID
import zio.*
import zio.test.*

object ExampleSpec extends HarnessSpec[ExampleSpec.Service1 & ExampleSpec.Service2 & ExampleSpec.Service3 & Proxy] {

  // =====| Models |=====

  final case class Person(
      id: UUID,
      firstName: String,
      lastName: String,
      age: Int,
  )

  sealed trait Animal
  object Animal {
    final case class Dog(id: UUID, ownerId: UUID, name: String) extends Animal
    final case class Bug(id: UUID, species: String, numLegs: Int) extends Animal
  }

  // =====| Services |=====

  trait Service1 {
    def getPerson: IO[String, Person]
  }

  trait Service2 {
    def getAnimal: IO[String, Animal]
  }

  final case class Service3(
      service1: Service1,
      service2: Service2,
  ) {

    def getPerson: IO[String, Person] = service1.getPerson

    def getAnimals(num: Int): IO[String, List[Animal]] = service2.getAnimal.replicateZIO(num).map(_.toList)

  }
  object Service3 {

    val layer: URLayer[Service1 & Service2, Service3] = ZLayer.fromFunction { Service3.apply }

  }

  // =====| Mocks |=====

  object Service1Mock extends Mock[Service1] {

    object GetPerson extends Effect[Unit, String, Person]

    override protected def buildInternal(proxy: Proxy): Service1 =
      new Service1 {
        override def getPerson: IO[String, Person] =
          proxy(GetPerson)
      }

  }

  object Service2Mock extends Mock[Service2] {

    object GetAnimal extends Effect[Unit, String, Animal]

    override protected def buildInternal(proxy: Proxy): Service2 =
      new Service2 {
        override def getAnimal: IO[String, Animal] =
          proxy(GetAnimal)
      }

  }

  // =====| Tests |=====

  override def layerProvider: LayerProvider[R] =
    LayerProvider.providePerTest(
      Proxy.layer,
      (
        Service1Mock.GetPerson.implement.success(Person(UUID.randomUUID, "first", "last", 18)) ++
          Service2Mock.empty
      ).toLayer,
      Service3.layer,
    )

  override def testSpec: TestSpec =
    suite("ExampleSpec")(
      test("test-1") {
        for {
          service3 <- ZIO.service[Service3]
          person <- service3.getPerson
          animal1 = Animal.Dog(UUID.randomUUID, person.id, "generic")
          animal2 = Animal.Bug(UUID.randomUUID, "centipede", 100)
          _ <- Service2Mock.GetAnimal.seed.success(animal1)
          _ <- Service2Mock.GetAnimal.seed.success(animal2)
          res <- service3.getAnimals(2)
        } yield assertTrue(
          res == List(animal1, animal2),
        )
      },
    )

}
