package es.weso.wshex.matcher
import munit._
import es.weso.wshex.WShExFormat._
import org.wikidata.wdtk.datamodel.helpers
import es.weso.utils.VerboseLevel
import org.wikidata.wdtk.datamodel.interfaces._
import org.wikidata.wdtk.datamodel.helpers._
import org.wikidata.wdtk.datamodel.helpers.ItemDocumentBuilder._
import org.wikidata.wdtk.datamodel.implementation.ItemIdValueImpl
import es.weso.wbmodel._
import cats.implicits._


/** Test matcher using Entity Schemas as input
  */
class WShExMatcherFullTest extends FunSuite {

  val defaultSite = "http://www.wikidata.org/entity/"
  val emptyDoc = forItemId(ItemIdValue.NULL)

  def Q(num: Long, site: String = defaultSite): ItemDocumentBuilder = {
    val id: ItemIdValue = new ItemIdValueImpl(s"Q$num", site)
    ItemDocumentBuilder.forItemId(id)
  }

  
  {
    val q42 = Q(42).build()

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<Human>
                       |
                       |<Human> { 
                       |}""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(Q(42).build()).some
    checkMatch("Label en with exact value and human", schemaStr, q42, expected)                       
  }

 
  def checkMatch(
      name: String,
      schemaStr: String,
      ed: EntityDocument,
      expected: Option[EntityDoc],  
      verboseLevel: VerboseLevel = VerboseLevel.Nothing
  )(implicit loc: munit.Location): Unit =
    test(name) {
     Matcher.unsafeFromString(
       str = schemaStr, 
       format = CompactWShExFormat, 
       verbose = verboseLevel).fold(
      parseError => fail(s"Error matching schema: $parseError"),
      matcher => {
        val matchStatus = matcher.matchStart(ed)
        if (verboseLevel.asBoolean) {
         println(s"Matcher schema = ${matcher.wShEx}") 
         val site: String = "http://www.wikidata.org/entity/" 
         val jsonDeserializer = new helpers.JsonDeserializer(site) 
         println(s"Entity Document: $ed")
         println(s"Match status = $matchStatus")
        }
        expected match {
          case None => matchStatus match {
           case nm: NoMatching => assertEquals(true,true)
           case m: Matching => fail(s"""|Expected to fail but passed with matching\n$m""".stripMargin)
          }
          case Some(ed) => matchStatus match {
           case nm: NoMatching => fail(s"""|Expected to pass but failed
                                           |Error: $nm
                                           |Expected: $ed
                                           |""".stripMargin)
           case m: Matching => assertEquals(m.entity, ed, 
            s"""|Value of matches != expected
                |Expected: $expected
                |Obtained=${matchStatus}
                |""".stripMargin)
        }

        }
      }
     )  
    }
}

