package es.weso.wshex.matcher

import es.weso.rbe.interval.Unbounded
import es.weso.rdf.nodes._
import es.weso.rdf._
import es.weso.wbmodel.PropertyId
import munit._
import org.wikidata.wdtk.datamodel.implementation._
import org.wikidata.wdtk.datamodel.helpers.ItemDocumentBuilder
import org.wikidata.wdtk.datamodel.helpers.StatementBuilder
import es.weso.wshex._
import es.weso.wbmodel.Value
import es.weso.wbmodel.Entity
import es.weso.wbmodel.EntityDoc
import es.weso.utils.VerboseLevel
import es.weso.wshex.TermConstraint._

object IRIHelpers {

  val wde = IRI("http://www.wikidata.org/entity/")

  def p(n: Int): IRI =
    wde + ("P" + n.toString)

  def q(n: Int): IRI =
    wde + ("Q" + n.toString)

}

class WShExTest extends CatsEffectSuite {
  import IRIHelpers._

/*  val shape = WShape(
    None,
    false,
    List(),
    Some(
      TripleConstraintLocal(
        PropertyId.fromIRI(IRI("http://www.wikidata.org/entity/P31")),
        ValueSet(None, List(IRIValueSetValue(IRI("http://www.wikidata.org/entity/Q515")))),
        1,
        Unbounded
      )
    ),
    List()
  )

  val schema: WSchema = WSchema(
    prefixes = None,
    shapesMap = Map(Start -> shape)
  )

  test("Match shape") {
    val q42 = new ItemIdValueImpl("Q42", "http://www.wikidata.org/entity/")
    val p31 = new PropertyIdValueImpl("P31", "http://www.wikidata.org/entity/")
    val q515 = new ItemIdValueImpl("Q515", "http://www.wikidata.org/entity/")
    val statementBuilder =
      StatementBuilder.forSubjectAndProperty(q42, p31).withValue(q515)
    val itemDocument =
      ItemDocumentBuilder.forItemId(q42).withStatement(statementBuilder.build()).build()
    assertEquals(
      Matcher(wShEx = schema).matchStart(itemDocument),
      Matching(List(shape), EntityDoc(itemDocument))
    )
  }

  test("Don't match shape when fails value") {
    val q42 = new ItemIdValueImpl("Q42", "http://www.wikidata.org/entity/")
    val p31 = new PropertyIdValueImpl("P31", "http://www.wikidata.org/entity/")
    val q515 = new ItemIdValueImpl("Q516", "http://www.wikidata.org/entity/")
    val statementBuilder =
      StatementBuilder.forSubjectAndProperty(q42, p31).withValue(q515)
    val itemDocument =
      ItemDocumentBuilder.forItemId(q42).withStatement(statementBuilder.build())
    assertEquals(
      Matcher(wShEx = schema).matchStart(itemDocument.build()).matches,
      false
    )
  }

  test("Don't match shape when fails property") {
    val q42 = new ItemIdValueImpl("Q42", "http://www.wikidata.org/entity/")
    val p31 = new PropertyIdValueImpl("P32", "http://www.wikidata.org/entity/")
    val q515 = new ItemIdValueImpl("Q515", "http://www.wikidata.org/entity/")
    val statementBuilder =
      StatementBuilder.forSubjectAndProperty(q42, p31).withValue(q515)
    val itemDocument =
      ItemDocumentBuilder.forItemId(q42).withStatement(statementBuilder.build())
    assertEquals(
      Matcher(wShEx = schema).matchStart(itemDocument.build()).matches,
      false
    )
  }

  test("Match shape when some value matches") {
    val q42 = new ItemIdValueImpl("Q42", "http://www.wikidata.org/entity/")
    val p31 = new PropertyIdValueImpl("P31", "http://www.wikidata.org/entity/")
    val q515 = new ItemIdValueImpl("Q515", "http://www.wikidata.org/entity/")
    val q516 = new ItemIdValueImpl("Q516", "http://www.wikidata.org/entity/")
    val s1 = StatementBuilder.forSubjectAndProperty(q42, p31).withValue(q515).build()
    val s2 = StatementBuilder.forSubjectAndProperty(q42, p31).withValue(q516).build()
    val itemDocument =
      ItemDocumentBuilder.forItemId(q42).withStatement(s1).withStatement(s2).build()
    val expectedItem = EntityDoc(ItemDocumentBuilder.forItemId(q42).withStatement(s1).build())
    assertEquals(
      Matcher(wShEx = schema).matchStart(itemDocument),
      Matching(List(shape), expectedItem)
    )
  } */

  {
   val shape = WShape(
    None,
    false,
    List(),
    None,
    List(LabelConstraint(Lang("en"),None))
   )

   val schema: WSchema = WSchema(
    prefixes = None,
    shapesMap = Map(Start -> shape)
   )
   val q5 = new ItemIdValueImpl("Q5", "http://www.wikidata.org/entity/")  
   val entity = EntityDoc(ItemDocumentBuilder.forItemId(q5).withLabel("Humano", "es").withLabel("Human", "en").build)
   val expected = EntityDoc(ItemDocumentBuilder.forItemId(q5).withLabel("Human", "en").build)
   matchTest("WShape with label", schema, entity, expected)
  }

  def matchTest(
   name: String, 
   schema: WSchema,
   entity: EntityDoc, 
   expected: EntityDoc
   )(implicit loc: munit.Location): Unit = {
    val matcher = Matcher(wShEx = schema)
    matcher.matchStart(entity.entityDocument) match {
        case nm: NoMatching => fail(s"No matching: $nm")
        case m: Matching => assertEquals(m.entity, expected)
      }
    }
}