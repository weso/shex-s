package es.weso.wshex

import es.weso.rdf.nodes.Lang
import org.wikidata.wdtk.datamodel.interfaces.TermedDocument
import org.wikidata.wdtk.datamodel.interfaces.MonolingualTextValue
import es.weso.utils.RegEx
import cats._
import cats.implicits._
import collection.JavaConverters._

/** TermConstraint describes constraints on terms: labels, descriptions or aliases
  */
sealed abstract class TermConstraint {
  def matchTerm(ed: TermedDocument): Either[String, Unit]
}

object TermConstraint {

  private def optMatchConstraint(
      maybesc: Option[StringConstraint],
      value: MonolingualTextValue
  ): Either[String, Unit] =
    maybesc match {
      case None     => ().asRight
      case Some(sc) => sc.matchMonolingualTextValue(value)
    }

  case class LabelConstraint(lang: Lang, strConstraint: Option[StringConstraint])
      extends TermConstraint {
    override def matchTerm(ed: TermedDocument): Either[String, Unit] = {
      val labelsMap = ed.getLabels()
      val value = labelsMap.get(lang.lang)
      if (value == null)
        s"LabelConstraintFailed: Not found value for lang $lang for term $ed".asLeft
      else optMatchConstraint(strConstraint, value)

    }
  }

  case class DescriptionConstraint(lang: Lang, strConstraint: Option[StringConstraint])
      extends TermConstraint {
    override def matchTerm(ed: TermedDocument): Either[String, Unit] = {
      val labelsMap = ed.getDescriptions()
      val value = labelsMap.get(lang.lang)
      if (value == null)
        s"DescriptionConstraintFailed: Not found value for lang $lang for term: $ed".asLeft
      else optMatchConstraint(strConstraint, value)
    }
  }

  case class AliasConstraint(lang: Lang, strConstraint: Option[StringConstraint])
      extends TermConstraint {
    override def matchTerm(ed: TermedDocument): Either[String, Unit] = {
      val labelsMap = ed.getAliases()
      val values = labelsMap.get(lang.lang)
      if (values == null)
        s"AliasConstraint failed: Not found value for lang $lang for term $ed".asLeft
      else {
        values.asScala.toList
          .map(v => optMatchConstraint(strConstraint, v))
          .sequence
          .map(_ => ())
      }
    }
  }

  case class AndTerms(ts: List[TermConstraint]) extends TermConstraint {
    override def matchTerm(ed: TermedDocument): Either[String, Unit] =
      ts.map(_.matchTerm(ed)).sequence.map(_ => ())
  }

  case class OrTerms(ts: List[TermConstraint]) extends TermConstraint {
    override def matchTerm(ed: TermedDocument): Either[String, Unit] = {
      val vs = ts.map(_.matchTerm(ed))
      if (vs.exists(_.isRight)) ().asRight
      else s"OrTerms failed: all terms failed matchTerm for term $ed\nTerms: $ts".asLeft
    }
  }

  case class NotTerm(t: TermConstraint) extends TermConstraint {
    override def matchTerm(ed: TermedDocument): Either[String, Unit] =
      t.matchTerm(ed) match {
        case Left(_)  => ().asRight
        case Right(_) => s"NotTerm failed: Term $ed passes constraint $t".asLeft
      }
  }

  sealed abstract class StringConstraint {
    def matchMonolingualTextValue(str: MonolingualTextValue): Either[String, Unit]
  }

  case class StrPattern(pattern: String, flags: Option[String]) extends StringConstraint {
    def matchMonolingualTextValue(value: MonolingualTextValue): Either[String, Unit] =
      RegEx(pattern, flags).matches(value.getText()) match {
        case Right(b) =>
          if (b) ().asRight
          else s"$value does not match regular expression /$pattern/^$flags".asLeft
        case Left(s) => s.asLeft

      }
  }

  case class Constant(str: String) extends StringConstraint {
    def matchMonolingualTextValue(value: MonolingualTextValue): Either[String, Unit] = {
      val s = value.getText()
      if (str == value.getText()) ().asRight
      else s"$str is different to $s".asLeft
    }
  }

}
