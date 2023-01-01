package template.api.generators

import zio.test.*

object Common {

  val nameGen: Gen[Sized, String] =
    for {
      start <- Gen.char('A', 'Z')
      end <- Gen.listOfBounded(4, 9)(Gen.char('a', 'z'))
    } yield (start :: end).mkString

}
