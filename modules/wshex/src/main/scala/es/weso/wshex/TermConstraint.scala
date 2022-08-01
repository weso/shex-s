package es.weso.wshex

import es.weso.rdf.nodes.Lang
import org.wikidata.wdtk.datamodel.interfaces.TermedDocument
import org.wikidata.wdtk.datamodel.interfaces.MonolingualTextValue
import es.weso.utils.RegEx
import cats._
import cats.implicits._
import collection.JavaConverters._
import es.weso.wshex.matcher.MatchingError
import es.weso.wshex.matcher.MatchingError._
import es.weso.wbmodel.EntityDoc

/** TermConstraint describes constraints on terms: labels, descriptions or aliases
  */
sealed abstract class TermConstraint {

  /** matchTerm attempts to match a termed document with a termConstraint
    * @param ed termed document to match
    * @param current current termed document that is being matched
    * @return Right(newterm) if matches adds, the matched term to current
    */
  def matchTerm(ed: EntityDoc, current: EntityDoc): Either[MatchingError, EntityDoc]
}

object TermConstraint {

  private def optMatchConstraint(
      maybesc: Option[StringConstraint],
      value: MonolingualTextValue
  ): Either[MatchingError, Unit] =
    maybesc match {
      case None     => ().asRight
      case Some(sc) => sc.matchMonolingualTextValue(value)
    }

  case class LabelConstraint(lang: Lang, strConstraint: Option[StringConstraint])
      extends TermConstraint {

    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = {
      val labelsMap = ed.getLabels()
      labelsMap.get(lang.lang) match {
        case None        => LabelConstraintNoLang(lang, ed).asLeft
        case Some(value) => optMatchConstraint(strConstraint, value).map(_ => current)
      }
    }

  }

  case class DescriptionConstraint(lang: Lang, strConstraint: Option[StringConstraint])
      extends TermConstraint {
    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = {
      val labelsMap = ed.getDescriptions()
      labelsMap.get(lang.lang) match {
        case None        => DescriptionConstraintNoLang(lang, ed).asLeft
        case Some(value) => optMatchConstraint(strConstraint, value).map(_ => current)
      }
    }
  }

  case class AliasConstraint(lang: Lang, strConstraint: Option[StringConstraint])
      extends TermConstraint {
    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = {
      val labelsMap = ed.getAliases()
      labelsMap.get(lang.lang) match {
        case None         => AliasConstraintNoLang(lang, ed).asLeft
        case Some(values) => ??? /* values.asScala.toList
          .map(v => optMatchConstraint(strConstraint, v))
          .sequence
          .map(_ => ()) */
      }
    }
  }

  case class AndTerms(ts: List[TermConstraint]) extends TermConstraint {

    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = ???
    // ts.map(_.matchTerm(ed, current)).sequence.map(_ => ())

  }

  case class OrTerms(ts: List[TermConstraint]) extends TermConstraint {

    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = ??? /* {
      val vs = ts.map(_.matchTerm(ed))
      if (vs.exists(_.isRight)) ().asRight
      else s"OrTerms failed: all terms failed matchTerm for term $ed\nTerms: $ts".asLeft
    } */
  }

  case class NotTerm(t: TermConstraint) extends TermConstraint {
    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = ???
    /* t.matchTerm(ed, currrent) match {
        case Left(_)  => current.asRight
        case Right(_) => s"NotTerm failed: Term $ed passes constraint $t".asLeft
      } */
  }

  sealed abstract class StringConstraint {
    def matchMonolingualTextValue(str: MonolingualTextValue): Either[MatchingError, Unit]
  }

  /*  case class StrPattern(pattern: String, flags: Option[String]) extends StringConstraint {
    def matchMonolingualTextValue(value: MonolingualTextValue): Either[MatchingError, Unit] =
      RegEx(pattern, flags).matches(value.getText()) match {
        case Right(b) =>
          if (b) ().asRight
          else RegexMatchingError(value, pattern, flags).asLeft
        case Left(s) => RegexMatchingError(value, pattern, flags, msg).asLeft

      }
  }
   */

  case class Constant(str: String) extends StringConstraint {
    def matchMonolingualTextValue(value: MonolingualTextValue): Either[MatchingError, Unit] = {
      val s = value.getText()
      if (str == value.getText()) ().asRight
      else StringConstantMatchingError(s, str).asLeft
    }
  }

}
