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
import es.weso.shex.StringFacet

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

  case class LabelAny(strConstraint: Option[StringConstraint])
      extends TermConstraint {

    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = {
      val labelsMap = ed.getLabels()
      if (labelsMap.isEmpty) LabelAnyNoLabel(ed).asLeft
      else labelsMap.toList.foldLeft(current.asRight[MatchingError]) { 
        case (c, pair) => {
          val (lang, value) = pair
          optMatchConstraint(strConstraint, value).flatMap((_:Unit) => c.map(cur => cur.withLabel(lang, value.getText())))
          /* for {
           _ <- optMatchConstraint(strConstraint, value)
           cur <- c
          } yield cur.withLabel(lang, value.getText()) */ 
        } 
      }
    }

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
        case Some(value) => 
         optMatchConstraint(strConstraint, value).map(_ => 
          current.withLabel(value.getLanguageCode(), value.getText()))
      }
    }

  }

  case class DescriptionAny(strConstraint: Option[StringConstraint])
      extends TermConstraint {
    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = {
      val descsMap = ed.getDescriptions()
      if (descsMap.isEmpty) DescrAnyNoDescr(ed).asLeft
      else descsMap.toList.foldLeft(current.asRight[MatchingError]) { 
        case (c, pair) => {
          val (lang, value) = pair
          for {
           _ <- optMatchConstraint(strConstraint, value)
           cur <- c
          } yield cur.withDescription(lang, value.getText()) 
        } 
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

  case class AliasAny(strConstraint: Option[StringConstraint]) extends TermConstraint {
    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = {
      val labelsMap = ed.getAliases()
      if (labelsMap.isEmpty)
        AliasAnyNoAlias(ed).asLeft
      else 
        labelsMap.toList.foldLeft(current.asRight[MatchingError]) { case (c, pair) => {
          val (lbl, as) = pair
          for {
           _ <- as.asScala.toList.map(optMatchConstraint(strConstraint, _)).sequence
           cur <- c
          } yield cur.withAliases(lbl, as.asScala.toList.map(_.getText()))
        }}
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
        case Some(values) => 
         values.asScala.toList.map(optMatchConstraint(strConstraint, _)).sequence.map(_ =>
          current.withAliases(lang.lang, values.asScala.toList.map(_.getText())) 
         )
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
    ): Either[MatchingError, EntityDoc] = ??? 
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

  case class Facet(facet: StringFacet) extends StringConstraint {
    import es.weso.shex.validator.FacetChecker
    import StringConstraintMatchError._

    def matchMonolingualTextValue(value: MonolingualTextValue): Either[MatchingError, Unit] = {
      val s = value.getText()
      FacetChecker.stringFacetChecker(s, facet).leftMap(err => 
        StringConstraintError(StringFacetMatchError(err), this, value)
      )
    }
  }   

  case class StringSet(ss: List[String]) extends StringConstraint {
    import StringConstraintMatchError._

    def matchMonolingualTextValue(value: MonolingualTextValue): Either[MatchingError, Unit] = {
      val s = value.getText()
      if (ss.contains(s)) ().asRight
      else StringConstraintError(StringSetMatchError(s, ss), this, value).asLeft
    }
  }   


  case class Constant(str: String) extends StringConstraint {
    def matchMonolingualTextValue(value: MonolingualTextValue): Either[MatchingError, Unit] = {
      val s = value.getText()
      if (str == value.getText()) ().asRight
      else StringConstantMatchingError(s, str).asLeft
    }
  }

  sealed abstract class StringConstraintMatchError 
  
  object StringConstraintMatchError {
    import es.weso.shex.validator.FacetChecker.StringFacetError
    case class StringFacetMatchError(err: StringFacetError) extends StringConstraintMatchError
    case class StringSetMatchError(value: String, ss: List[String]) extends StringConstraintMatchError
  }


}
