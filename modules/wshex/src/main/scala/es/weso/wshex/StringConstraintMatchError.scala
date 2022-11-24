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

sealed abstract class StringConstraintMatchError 
  
object StringConstraintMatchError {
  import es.weso.shex.validator.FacetChecker.StringFacetError
  
  case class StringFacetMatchError(err: StringFacetError) extends StringConstraintMatchError
  case class StringSetMatchError(value: String, ss: List[String]) extends StringConstraintMatchError
}
