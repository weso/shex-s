package es.weso.shex.validator

import es.weso.rdf.nodes._
import es.weso.shex._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpecLike
import org.scalatest.matchers.should.Matchers
import es.weso.rdf.jena.RDFAsJenaModel
import cats.effect.IO

class ValueCheckerTest extends AnyFunSpecLike with Matchers with EitherValues {

  describe("LanguageStem") {
    valueCheckerTest(LangLiteral("pepe", Lang("frc")),
      LanguageStemRange(LanguageStemRangeLang(Lang("fr")),Some(List())), false)
    valueCheckerTest(LangLiteral("pepe", Lang("fr-cc")),
        LanguageStemRange(LanguageStemRangeLang(Lang("fr")),Some(List())), true)
    valueCheckerTest(LangLiteral("pepe", Lang("fr-be")), LanguageStem(Lang("fr")), true)
    valueCheckerTest(LangLiteral("pepe", Lang("fr-FR")),
      LanguageStemRange(LanguageStemRangeLang(Lang("fr")),
          Some(List(LanguageTagExclusion(Lang("fr-be")),
            LanguageTagExclusion(Lang("fr-cd")),
            LanguageTagExclusion(Lang("fr-ch")))))
      , true)
    valueCheckerTest(LangLiteral("septante", Lang("frc")),
      LanguageStemRange(LanguageStemRangeLang(Lang("")),
        Some(List(LanguageStemExclusion(LanguageStem(Lang("fr-be"))),
          LanguageStemExclusion(LanguageStem(Lang("fr-cd"))),
          LanguageStemExclusion(LanguageStem(Lang("fr-ch"))))))
      , true)
    valueCheckerTest(LangLiteral("septante", Lang("fr-bel")),
      LanguageStemRange(LanguageStemRangeLang(Lang("fr")),
        Some(List(LanguageStemExclusion(LanguageStem(Lang("fr-be"))),
          LanguageStemExclusion(LanguageStem(Lang("fr-cd"))),
          LanguageStemExclusion(LanguageStem(Lang("fr-ch"))))))
      , true)
  }

  def valueCheckerTest(node: RDFNode, value: ValueSetValue, ok: Boolean): Unit = {
    it(s"should checkValue($node, $value) and return $ok") {
      val cmp = RDFAsJenaModel.empty.flatMap(_.use(builder => {
        val vck = ValueChecker(Schema.empty,builder)
        IO(if (ok) vck.valueChecker(node,value).fold(
         e => Left(s"Should check, but failed with message: $e"),
         msg => Right(s"Passed with message: $msg")
        ) else vck.valueChecker(node,value).fold(
         e => Right(s"Should fail and failed with message: $e"),
         msg => Left(s"Should fail but passed with message: $msg")
        ))
      }))
      cmp.unsafeRunSync().fold(err => fail(err), v => info(v))
    }
  }
}
