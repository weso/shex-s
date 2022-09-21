package es.weso.wshex.matcher
import munit._
import es.weso.wshex.WShExFormat._
import org.wikidata.wdtk.datamodel.helpers
import es.weso.utils.VerboseLevel
import org.wikidata.wdtk.datamodel.interfaces._
import org.wikidata.wdtk.datamodel.helpers._
import org.wikidata.wdtk.datamodel.helpers.ItemDocumentBuilder._
import org.wikidata.wdtk.datamodel.implementation._
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

    {
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val p31_q5 = 
      StatementBuilder.forSubjectAndProperty(q42_raw.getEntityId(), 
        PropertyIdValueImpl("P31", defaultSite)).withValue(q5.getEntityId()).build()
    val p19_q6 = 
      StatementBuilder.forSubjectAndProperty(q42_raw.getEntityId(), 
        PropertyIdValueImpl("P19", defaultSite)).withValue(Q(6).build().getEntityId()).build()
    val q42_p31_q5 = q42_raw.withStatement(p31_q5)
    val q42_full = q42_p31_q5.withStatement(p19_q6)


    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<Human>
                       |
                       |<Human> { 
                       |  :P31 [ :Q5 ]
                       |}""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_p31_q5).some
    checkMatch("Label en with exact value and human", schemaStr, q42_full, expected)
  }

  {
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val douglas = StringValueImpl("Douglas Adams")
    val p31_q5 = 
      StatementBuilder.forSubjectAndProperty(q42_raw.getEntityId(), 
        PropertyIdValueImpl("P31", defaultSite)).withValue(q5.getEntityId()).build()
    val p734_adams = 
      StatementBuilder.forSubjectAndProperty(q42_raw.getEntityId(), 
        PropertyIdValueImpl("P734", defaultSite)).withValue(StringValueImpl("Adams")).build()
    val q42_p734_adams = q42_raw.withStatement(p734_adams)
    val q42_full = q42_p734_adams.withStatement(p31_q5)


    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<Human>
                       |
                       |<Douglas> { 
                       |  :P734 /Ad*/ ;
                       |}""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_p734_adams).some
    checkMatch("Label en with exact value and human", schemaStr, q42_full, expected)
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

