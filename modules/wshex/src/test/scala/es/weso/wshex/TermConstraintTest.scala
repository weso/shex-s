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
import es.weso.wbmodel.EntityDoc
import es.weso.wshex.matcher.MatchingError
import es.weso.wshex.matcher.MatchingError._
import cats.implicits._

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
      LabelConstraint(Map(Lang("en") -> Some(Constant("Douglas Adams")))),
      EntityDoc(itemDocument).asRight
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
      "Q42 with lang es should fail",
      itemDocument,
      LabelConstraint(Map(Lang("es") -> Some(Constant("Douglas Adams")))),
      NoLang(lang = "es", mode = LabelMode, entity = EntityDoc(itemDocument)).asLeft
    )
  }

    {
    val q5 = new ItemIdValueImpl("Q42", "http://www.wikidata.org/entity/")
    val itemDocument =
      ItemDocumentBuilder
        .forItemId(q5)
        .withLabel(Datamodel.makeMonolingualTextValue("Human", "en"))
        .withLabel(Datamodel.makeMonolingualTextValue("Humano", "es"))
        .build()
    val expected = 
      EntityDoc(ItemDocumentBuilder
        .forItemId(q5)
        .withLabel(Datamodel.makeMonolingualTextValue("Humano", "es"))
        .build())   
    checkTerm(
      "Q5 with lang es should pass",
      itemDocument,
      LabelConstraint(Map(Lang("es") -> None)),
      expected.asRight
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
      "Q42 with constant string Doug should fail",
      itemDocument,
      LabelConstraint(Map(Lang("en") -> Some(Constant("Doug")))),
      StringConstantMatchingError(s = "Douglas Adams", expected = "Doug").asLeft
    )
  }

  def checkTerm(
      name: String,
      td: TermedDocument,
      tc: TermConstraint,
      expected: Either[MatchingError, EntityDoc]
  )(implicit loc: munit.Location): Unit =
    test(name) {
      assertEquals(tc.matchTerm(EntityDoc(td), EntityDoc.emptyFrom(td)), expected) 
    }
}
