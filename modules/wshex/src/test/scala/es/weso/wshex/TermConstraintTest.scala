package es.weso.wshex
import munit._
import io.circe.Json
import es.weso.rdf.nodes.IRI
import es.weso.rdf.PrefixMap
import es.weso.rbe.interval.IntLimit
import es.weso.wbmodel.EntityId
import es.weso.wbmodel.PropertyId
import es.weso.utils.VerboseLevel._
import es.weso.shex.{Schema => ShExSchema}
import es.weso.rbe.interval._
import org.wikidata.wdtk.datamodel.interfaces.TermedDocument
import org.wikidata.wdtk.datamodel.implementation.ItemIdValueImpl
import org.wikidata.wdtk.datamodel.helpers.ItemDocumentBuilder
import org.wikidata.wdtk.datamodel.helpers.Datamodel
import TermConstraint._
import es.weso.rdf.nodes.Lang

class TermConstraintTest extends CatsEffectSuite {

  {
    val q42 = new ItemIdValueImpl("Q42", "http://www.wikidata.org/entity/")
    val itemDocument =
      ItemDocumentBuilder
        .forItemId(q42)
        .withLabel(Datamodel.makeMonolingualTextValue("Douglas Adams", "en"))
        .build()
    checkTerm(
      "Basic label Q42",
      itemDocument,
      LabelConstraint(Lang("en"), Some(Constant("Douglas Adams"))),
      true
    )
  }

  {
    val q42 = new ItemIdValueImpl("Q42", "http://www.wikidata.org/entity/")
    val itemDocument =
      ItemDocumentBuilder
        .forItemId(q42)
        .withLabel(Datamodel.makeMonolingualTextValue("Douglas Adams", "en"))
        .build()
    checkTerm(
      "Basic label Q42",
      itemDocument,
      LabelConstraint(Lang("es"), Some(Constant("Douglas Adams"))),
      false
    )
  }

  {
    val q42 = new ItemIdValueImpl("Q42", "http://www.wikidata.org/entity/")
    val itemDocument =
      ItemDocumentBuilder
        .forItemId(q42)
        .withLabel(Datamodel.makeMonolingualTextValue("Douglas Adams", "en"))
        .build()
    checkTerm(
      "Basic label Q42",
      itemDocument,
      LabelConstraint(Lang("en"), Some(Constant("Doug"))),
      false
    )
  }

  def checkTerm(
      name: String,
      td: TermedDocument,
      tc: TermConstraint,
      expected: Boolean
  )(implicit loc: munit.Location): Unit =
    test(name) {
      tc.matchTerm(td) match {
        case Left(s) =>
          if (expected) fail(s"Matching term $td with term constraint $tc: $s")
          else ()
        case Right(_) =>
          if (expected) ()
          else fail(s"Matching term $td with term constraint $tc passes but was expected to fail")
      }
    }
}
