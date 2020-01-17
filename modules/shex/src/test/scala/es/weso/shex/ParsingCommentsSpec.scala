package es.weso.shex

import org.scalatest.EitherValues

import scala.io.Source
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ParsingCommentsSpec extends AnyFlatSpec with Matchers with EitherValues {
  "Shex" should "parse comments" in {
    val contentFile = Source.fromFile("modules/shex/src/test/resources/localTests/shex-parsing-of-comments.shex")
    val parsing     = Schema.fromString(contentFile.getLines().mkString, "ShexC", None)
    parsing match {
      case Right(_) => succeed
      case Left(e)  => fail(e)
    }
  }
}
