package es.weso.wshex

import es.weso.rdf.nodes.Lang
import org.wikidata.wdtk.datamodel.interfaces.TermedDocument
import org.wikidata.wdtk.datamodel.interfaces.MonolingualTextValue
import es.weso.utils.RegEx
import cats._
import cats.implicits._
// import collection.JavaConverters._
import es.weso.wshex.matcher.MatchingError
import es.weso.wshex.matcher.MatchingError._
import es.weso.wbmodel.EntityDoc
import es.weso.shex.StringFacet
import es.weso.utils.internal.CollectionCompat._

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

sealed abstract class TermMode
case object LabelMode extends TermMode
case object DescriptionMode extends TermMode
case object AliasesMode extends TermMode

object TermConstraint {

  private def matchStrLangsMap(
    langsMap: Map[String, MonolingualTextValue], 
    strLang: String, 
    strConstraint: Option[StringConstraint],
    termMode: TermMode,
    ed: EntityDoc
    ): Either[MatchingError, Option[MonolingualTextValue]] = {
      langsMap.get(strLang) match {
        case None => strConstraint match {
          case None => none.asRight 
          case Some(_) => NoLang(strLang, termMode, ed).asLeft
        }
        case Some(txt) => 
          optMatchConstraint(strConstraint, txt).map(_.some)
      }
    }

    private def matchStrLangsMapLs(
    langsMap: Map[String, List[MonolingualTextValue]], 
    strLang: String, 
    strConstraint: Option[StringConstraint],
    termMode: TermMode,
    ed: EntityDoc
    ): Either[MatchingError, Option[List[MonolingualTextValue]]] = {
      langsMap.get(strLang) match {
        case None => strConstraint match {
          case None => none.asRight 
          case Some(_) => NoLang(strLang, termMode, ed).asLeft
        }
        case Some(txts) => 
          optMatchConstraints(strConstraint, txts).map(_.some)
      }
    }
  

  private def matchLangsMap(
    langsMap: Map[String, MonolingualTextValue], 
    constraintsMap: Map[Lang, Option[StringConstraint]],
    termMode: TermMode,
    ed: EntityDoc
    ): Either[MatchingError, Map[String, MonolingualTextValue]] = {
      val zero:Either[MatchingError, Map[String,MonolingualTextValue]] = Map[String, MonolingualTextValue]().asRight
      def cmb(
         pair: (Lang, Option[StringConstraint]), 
         current: Either[MatchingError, Map[String,MonolingualTextValue]]
         ): Either[MatchingError, Map[String,MonolingualTextValue]] = {
        val (lang, strConst) = pair
        val strLang: String = lang.lang
        for { 
          maybeStr <- matchStrLangsMap(langsMap, strLang, strConst, termMode, ed)
          currentMap <- current
        } yield maybeStr match { 
          case None => currentMap
          case Some(txt) => currentMap.updated(strLang, txt)
        }
      }
      constraintsMap.toList.foldRight(zero)(cmb)
    }

  private def matchLangsMapLs(
    langsMapLs: Map[String, List[MonolingualTextValue]], 
    constraintsMap: Map[Lang, Option[StringConstraint]],
    termMode: TermMode,
    ed: EntityDoc
    ): Either[MatchingError, Map[String, List[MonolingualTextValue]]] = {
      val zero: Either[MatchingError, Map[String, List[MonolingualTextValue]]] = Map[String, List[MonolingualTextValue]]().asRight
      def cmb(
         pair: (Lang, Option[StringConstraint]), 
         current: Either[MatchingError, Map[String,List[MonolingualTextValue]]]
         ): Either[MatchingError, Map[String,List[MonolingualTextValue]]] = {
        val (lang, strConst) = pair
        val strLang: String = lang.lang
        for { 
          maybeStr <- matchStrLangsMapLs(langsMapLs, strLang, strConst, termMode, ed)
          currentMap <- current
        } yield maybeStr match { 
          case None => currentMap
          case Some(txt) => currentMap.updated(strLang, txt)
        }
      }
      constraintsMap.toList.foldRight(zero)(cmb)
  }


  private def addMapValues(
    c: EntityDoc, 
    newValues: Map[String, MonolingualTextValue], 
    updateFn: (EntityDoc, String, MonolingualTextValue
    ) => EntityDoc): EntityDoc = {
      def cmb(pair: (String, MonolingualTextValue), c: EntityDoc): EntityDoc = {
        val (s,txt) = pair
        updateFn(c,s,txt)
      }
      newValues.toList.foldRight(c)(cmb)
    }

  private def addMapValuesLs(
    c: EntityDoc, 
    newValues: Map[String, List[MonolingualTextValue]], 
    updateFn: (EntityDoc, String, List[MonolingualTextValue]
    ) => EntityDoc): EntityDoc = {
      def cmb(pair: (String, List[MonolingualTextValue]), c: EntityDoc): EntityDoc = {
        val (s,txts) = pair
        updateFn(c,s,txts)
      }
      newValues.toList.foldRight(c)(cmb)
    }

  private def optMatchConstraint(
      maybesc: Option[StringConstraint],
      value: MonolingualTextValue
  ): Either[MatchingError, MonolingualTextValue] =
    maybesc match {
      case None     => value.asRight
      case Some(sc) => sc.matchMonolingualTextValue(value)
    }

  private def optMatchConstraints(
      maybesc: Option[StringConstraint],
      values: List[MonolingualTextValue]
  ): Either[MatchingError, List[MonolingualTextValue]] =
    maybesc match {
      case None     => values.asRight
      case Some(sc) => values.foldRight(List[MonolingualTextValue]().asRight[MatchingError]){
        case (v, current) => for {
          v1 <- sc.matchMonolingualTextValue(v)
          vs <- current
        } yield v1 +: vs
      }
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
          optMatchConstraint(strConstraint, value).flatMap(v => 
            c.map(cur => cur.withLabel(lang, v.getText())))
        } 
      }
    }

  }

  case class LabelConstraint(constraintsMap: Map[Lang, Option[StringConstraint]])
      extends TermConstraint {

    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = {
      val labelsMap = ed.getLabels()
      matchLangsMap(labelsMap, constraintsMap, LabelMode, ed).map(
        addMapValues(current, _, (e,s,v) => e.withLabel(s,v.getText()))
      )
/*      labelsMap.get(lang.lang) match {
        case None        => LabelConstraintNoLang(lang, ed).asLeft
        case Some(value) => 
         optMatchConstraint(strConstraint, value).map(_ => 
          current.withLabel(value.getLanguageCode(), value.getText()))
      } */
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

 case class DescriptionConstraint(constraintsMap: Map[Lang, Option[StringConstraint]])
      extends TermConstraint {
    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = {
      val descsMap = ed.getDescriptions()
      matchLangsMap(descsMap, constraintsMap, DescriptionMode, ed).map(
        addMapValues(current, _, (e,s,v) => e.withDescription(s,v.getText()))
      )
/*      labelsMap.get(lang.lang) match {
        case None        => DescriptionConstraintNoLang(lang, ed).asLeft
        case Some(value) => optMatchConstraint(strConstraint, value).map(_ => current)
      } */
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
           _ <- as.map(optMatchConstraint(strConstraint, _)).sequence
           cur <- c
          } yield cur.withAliases(lbl, as.map(_.getText()))
        }}
    }
  }

  case class AliasConstraint(constraintsMap: Map[Lang, Option[StringConstraint]])
      extends TermConstraint {
    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = {
      val aliasesMap = ed.getAliases()
      matchLangsMapLs(aliasesMap, constraintsMap, AliasesMode, ed).map(
        addMapValuesLs(current, _, (e,s,vs) => e.withAliases(s,vs.map(_.getText())))
      )
     } 
  }

  case class AndTerms(ts: List[TermConstraint]) extends TermConstraint {

    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = 
    Pending(s"Not implemented match AndTerms yet for termConstraints: $ts\nEntityDoc: $ed").asLeft

  }

  case class OrTerms(ts: List[TermConstraint]) extends TermConstraint {

    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = 
    Pending(s"Not implemented match OrTerms yet for termConstraints: $ts\nEntityDoc: $ed").asLeft

  }

  case class NotTerm(t: TermConstraint) extends TermConstraint {
    override def matchTerm(
        ed: EntityDoc,
        current: EntityDoc
    ): Either[MatchingError, EntityDoc] = 
      Pending(s"Not implemented match NotTerm yet for termConstraint: $t").asLeft
    /* t.matchTerm(ed, currrent) match {
        case Left(_)  => current.asRight
        case Right(_) => s"NotTerm failed: Term $ed passes constraint $t".asLeft
      } */
  }


}