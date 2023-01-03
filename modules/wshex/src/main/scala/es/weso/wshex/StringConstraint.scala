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
import es.weso.utils.internal.CollectionCompat._

sealed abstract class StringConstraint {
    def matchMonolingualTextValue(str: MonolingualTextValue): Either[MatchingError, MonolingualTextValue]
}

case class Facet(facet: StringFacet) extends StringConstraint {
    import es.weso.shex.validator.FacetChecker
    import StringConstraintMatchError._

    override def matchMonolingualTextValue(
      value: MonolingualTextValue
    ): Either[MatchingError, MonolingualTextValue] = {
      val s = value.getText()
      FacetChecker.stringFacetChecker(s, facet).bimap(
        err => StringConstraintError(StringFacetMatchError(err), this, value),
        _ => value
      )
/*      leftMap(
      ).map(_ => value) */
    }
  }   

 case class StringSet(ss: List[String]) extends StringConstraint {
    import StringConstraintMatchError._

    def matchMonolingualTextValue(value: MonolingualTextValue): Either[MatchingError, MonolingualTextValue] = {
      val s = value.getText()
      if (ss.contains(s)) value.asRight
      else StringConstraintError(StringSetMatchError(s, ss), this, value).asLeft
    }
  }   


 case class Constant(str: String) extends StringConstraint {
    def matchMonolingualTextValue(value: MonolingualTextValue): Either[MatchingError, MonolingualTextValue] = {
      val s = value.getText()
      if (str == value.getText()) value.asRight
      else StringConstantMatchingError(s, str).asLeft
    }
  }

