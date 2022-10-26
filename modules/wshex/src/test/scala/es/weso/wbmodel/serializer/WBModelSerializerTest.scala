package es.weso.wbmodel.serializer

import munit.CatsEffectSuite
import org.wikidata.wdtk.datamodel.interfaces.EntityDocument
import es.weso.rdf.jena.RDFAsJenaModel
import cats.effect._
import org.wikidata.wdtk.datamodel.implementation.ItemDocumentImpl
import org.wikidata.wdtk.datamodel.implementation.MonolingualTextValueImpl
import org.wikidata.wdtk.datamodel.helpers.StatementBuilder
import org.wikidata.wdtk.datamodel.implementation.PropertyIdValueImpl
import org.wikidata.wdtk.datamodel.interfaces.ItemIdValue
import org.wikidata.wdtk.datamodel.implementation.ItemIdValueImpl
import org.wikidata.wdtk.datamodel.helpers.ItemDocumentBuilder
import org.wikidata.wdtk.datamodel.helpers.Datamodel._
import org.wikidata.wdtk.datamodel.implementation.StringValueImpl
import org.wikidata.wdtk.datamodel.helpers.ReferenceBuilder
import org.wikidata.wdtk.datamodel.implementation.ValueSnakImpl
import es.weso.utils.test._

class WBModelSerializerTest extends CatsEffectSuite {

  val defaultSite = "http://www.wikidata.org/entity/"
  def Q(num: Long, site: String = defaultSite): ItemDocumentBuilder = {
    val id: ItemIdValue = new ItemIdValueImpl(s"Q$num", site)
    ItemDocumentBuilder.forItemId(id)
  }

  {
   val q42 = Q(42).build() 
   val q5 = Q(5).build() 
   val q6 = Q(6).build()
   val hi = new StringValueImpl("Hi")
   val ref1 = ReferenceBuilder.newInstance()
        .withPropertyValue(PropertyIdValueImpl("P214", defaultSite), hi)
        .withPropertyValue(PropertyIdValueImpl("P248", defaultSite), q6.getEntityId())
        .build()
   val p31_q5 = 
      StatementBuilder.forSubjectAndProperty(q42.getEntityId(), 
        new PropertyIdValueImpl("P31", defaultSite)).withValue(q5.getEntityId()
      ).withQualifierValue(PropertyIdValueImpl("P585", defaultSite), makeStringValue("Hi")).withReference(ref1)
      .build()
   val q42_p31_q5 = q42.withStatement(p31_q5)     
   checkSerialization("Simple statement",
     q42_p31_q5,
     """|prefix wd: <http://www.wikidata.org/entity/>
        |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
        |
        |wd:Q42 wdt:P31 wd:Q5 .
        |""".stripMargin,
     IgnoreTest
   )
  }  
 
  def checkSerialization(
      name: String,
      entityDocument: EntityDocument,
      expected: String,
      shouldIgnore: ShouldIgnoreOption
  )(implicit loc: munit.Location): Unit = 
    shouldIgnore match {
        case IgnoreTest => test(name.ignore) { }
        case DontIgnore => test(name) {
        RDFAsJenaModel.fromString(expected, "TURTLE").flatMap(_.use(rdfExpected => 
            rdfExpected.serialize("TURTLE").flatMap(strExpected => 
            RDFSerializer("TURTLE").serialize(entityDocument).flatMap(entityDocStr => 
                IO.pure((entityDocStr, RDFSerializer.removePrefixes(strExpected)))
        )))).map { 
            case (entityDocStr, strExpected) => assertEquals(entityDocStr, strExpected)
        }
     }
    }
}
