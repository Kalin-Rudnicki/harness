package harness.xml

import cats.syntax.option.*
import harness.zio.test.*
import scala.xml.XML
import zio.test.*
import zio.test.Assertion.*

object XmlDecoderSpec extends DefaultHarnessSpec {

  private final case class Company(
      name: String,
      foundedYear: Int,
      employees: List[Person],
  )
  private object Company {

    val xmlDecoder: XmlDecoder.SingleNodeDecoder[Company] =
      (
        XmlDecoder.textFromStringDecoder[String].inNode("Name") <*>
          XmlDecoder.textFromStringDecoder[Int].inNode("FoundedYear") <*>
          Person.xmlDecoder.list.inNode("Employees")
      ).inNode("Company").map(Company.apply)

  }

  private final case class Person(
      firstName: String,
      lastName: String,
      age: Option[Int],
  )
  private object Person {

    val xmlDecoder: XmlDecoder.SingleNodeDecoder[Person] =
      (
        XmlDecoder.textFromStringDecoder[String].inNode("FirstName") <*>
          XmlDecoder.textFromStringDecoder[String].inNode("LastName") <*>
          XmlDecoder.textFromStringDecoder[Int].inNode("Age").optional
      ).inNode("Person").map(Person.apply)

  }

  private def makePassingTest[T](name: String)(xml: String, decoder: XmlDecoder[T], exp: T): TestSpec =
    test(name) {
      assert(decoder.decodeAccumulating(Seq(XML.loadString(xml))))(isRight(equalTo(exp)))
    }

  override def testSpec: TestSpec =
    suite("XmlDecoderSpec")(
      suite("passes")(
        makePassingTest("person-1")(
          <Person>
            <FirstName>F</FirstName>
            <LastName>L</LastName>
            <Age>25</Age>
          </Person>.toString,
          Person.xmlDecoder,
          Person("F", "L", 25.some),
        ),
        makePassingTest("person-2")(
          <Person>
            <FirstName>F</FirstName>
            <LastName>L</LastName>
          </Person>.toString,
          Person.xmlDecoder,
          Person("F", "L", None),
        ),
        makePassingTest("company-1")(
          <Company>
            <Name>C</Name>
            <FoundedYear>2000</FoundedYear>
            <Employees>
              <Person>
                <FirstName>F1</FirstName>
                <LastName>L1</LastName>
                <Age>25</Age>
              </Person>
              <Person>
                <FirstName>F2</FirstName>
                <LastName>L2</LastName>
              </Person>
            </Employees>
          </Company>.toString,
          Company.xmlDecoder,
          Company("C", 2000, List(Person("F1", "L1", 25.some), Person("F2", "L2", None))),
        ),
      ),
    )

}
