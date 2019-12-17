package es.weso.shex

import org.scalatest.{EitherValues, FlatSpec, Matchers}

import scala.io.Source

class ParsingCommentsSpec extends FlatSpec with Matchers with EitherValues {
  "Shex" should "parse comments" in {
    val contentFile = Source.fromFile("modules/shex/src/test/resources/localTests/shex-parsing-of-comments.shex")
    val parsing     = Schema.fromString(contentFile.getLines().mkString, "ShexC", None)
    parsing match {
      case Right(_) => succeed
      case Left(e)  => fail(e)
    }
  }
}
