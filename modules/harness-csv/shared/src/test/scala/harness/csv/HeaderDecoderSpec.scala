package harness.csv

import cats.syntax.option.*
import harness.zio.test.*
import zio.test.*
import zio.test.Assertion.*

object HeaderDecoderSpec extends DefaultHarnessSpec {

  private def passingTest[T: CsvHeaderDecoder](name: String)(csv: String, exp: List[T]): TestSpec =
    test(name) {
      assert(CsvHeaderDecoder.decode[T](csv))(isRight(equalTo(exp)))
    }

  private final case class Person(
      firstName: String,
      lastName: String,
      age: Option[Int],
  )

  private object Person {

    implicit val csvDecoder: CsvHeaderDecoder[Person] =
      (
        CsvHeaderDecoder.cell[String]("first-name") ++
          CsvHeaderDecoder.cell[String]("last-name") ++
          CsvHeaderDecoder.cell[Int]("age").optional
      ).map(Person.apply)

  }

  override def testSpec: TestSpec =
    suite("DecoderSpec")(
      suite("passes")(
        passingTest[Person]("case-1")("first-name,last-name,age\nF,L,", List(Person("F", "L", None))),
        passingTest[Person]("case-2")("first-name,last-name,age\nF,L,0", List(Person("F", "L", 0.some))),
        passingTest[Option[Person]]("case-3")("first-name,last-name,age\nF,L,\nF,L,0\n,,", List(Person("F", "L", None).some, Person("F", "L", 0.some).some, None)),
        passingTest[Person]("case-1")("last-name,first-name,age\nL,F,", List(Person("F", "L", None))),
      ),
    )

}
