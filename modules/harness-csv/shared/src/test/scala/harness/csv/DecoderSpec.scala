package harness.csv

import cats.syntax.option.*
import harness.zio.test.*
import zio.test.*
import zio.test.Assertion.*

object DecoderSpec extends DefaultHarnessSpec {

  private def passingTest[T: CsvDecoder](name: String)(csv: String, exp: List[T]): TestSpec =
    test(name) {
      assert(CsvDecoder.decode[T](csv, false))(isRight(equalTo(exp)))
    }

  private final case class Person(
      firstName: String,
      lastName: String,
      age: Option[Int],
  )
  private object Person {

    implicit val csvDecoder: CsvDecoder[Person] =
      CsvDecoder[String][String][Option[Int]].map(Person.apply)

  }

  override def testSpec: TestSpec =
    suite("DecoderSpec")(
      suite("passes")(
        passingTest[Person]("case-1")("F,L,", List(Person("F", "L", None))),
        passingTest[Person]("case-2")("F,L,0", List(Person("F", "L", 0.some))),
        passingTest[Option[Person]]("case-3")("F,L,\nF,L,0\n,,", List(Person("F", "L", None).some, Person("F", "L", 0.some).some, None)),
      ),
    )

}
