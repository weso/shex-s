package es.weso.wshex.matcher
import munit._
import es.weso.wshex.WShExFormat._
import org.wikidata.wdtk.datamodel.helpers
import es.weso.utils.VerboseLevel
import es.weso.wshex.WShExFormat

/** Test matcher using Entity Schemas as input
  */
class WShExMatcherBooleanTest extends FunSuite {

  val q42Str =
      """|{"type":"item","aliases":{},"labels":{"en":{"language":"en","value":"Douglas Adams"}},"descriptions":{},"sitelinks":{},"id":"Q42","claims":{"P31":[{"rank":"normal","references":[{"snaks":{"P214":[{"snaktype":"value","property":"P214","datavalue":{"type":"string","value":"113230702"}}],"P248":[{"snaktype":"value","property":"P248","datavalue":{"type":"wikibase-entityid","value":{"entity-type":"item","numeric-id":54919}},"datatype":"wikibase-item"}],"P813":[{"snaktype":"value","property":"P813","datavalue":{"type":"time","value":{"time":"+00000002013-12-07T00:00:00Z","timezone":0,"before":0,"after":0,"precision":11,"calendarmodel":"http://www.wikidata.org/entity/Q1985727"}},"datatype":"time"}]},"allSnaks":[{"property":"P248","datavalue":{"type":"wikibase-entityid","value":{"entity-type":"item","numeric-id":54919}},"datatype":"wikibase-item"},{"property":"P214","datavalue":{"type":"string","value":"113230702"}},{"property":"P813","datavalue":{"type":"time","value":{"time":"+00000002013-12-07T00:00:00Z","timezone":0,"before":0,"after":0,"precision":11,"calendarmodel":"http://www.wikidata.org/entity/Q1985727"}},"datatype":"time"}],"snaks-order":["P248","P214","P813"]}],"mainsnak":{"snaktype":"value","property":"P31","datavalue":{"type":"wikibase-entityid","value":{"entity-type":"item","numeric-id":5}},"datatype":"wikibase-item"},"id":"Q42$F078E5B3-F9A8-480E-B7AC-D97778CBBEF9","type":"statement"}]}}""".stripMargin

  {
    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<Human>
                       |
                       |<Human> LABEL ( en -> "Douglas Adams" ) {
                       |  :P31 [ :Q5 ]
                       |}""".stripMargin

    checkMatch("Label en with exact value and human", schemaStr, q42Str, true)                       
  }

  {
    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<Human>
                       |
                       |<Human> LABEL ( en -> . ) {
                       |  :P31 [ :Q5 ]
                       |}""".stripMargin

    checkMatch("Label en and human", schemaStr, q42Str, true)                       
  }

  { // TODO: Maybe we should revisit this test...
    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<Human>
                       |
                       |<Human> LABEL ( es -> . ) {
                       |  :P31 [ :Q5 ]
                       |}""".stripMargin

    checkMatch(name = "Label es and human", schemaStr = schemaStr, jsonStr = q42Str, expected = true) 
  }

  def checkMatch(
      name: String,
      schemaStr: String,
      jsonStr: String,
      expected: Boolean, 
      verboseLevel: VerboseLevel = VerboseLevel.Nothing,
      schemaFormat: WShExFormat = WShExFormat.CompactWShExFormat
  )(implicit loc: munit.Location): Unit =
    test(name) {
     Matcher.unsafeFromString(
       str = schemaStr, 
       format = CompactWShExFormat, 
       verbose = verboseLevel).fold(
      parseError => fail(s"Error matching schema: $parseError"),
      matcher => {
        val matchStatus = matcher.matchJsonStart(jsonStr)
        if (verboseLevel.asBoolean) {
         println(s"Matcher schema = ${matcher.wShEx}") 
         val site: String = "http://www.wikidata.org/entity/" 
         val jsonDeserializer = new helpers.JsonDeserializer(site) 
         val entityDocument = jsonDeserializer.deserializeEntityDocument(jsonStr)
         println(s"Entity Document: $entityDocument")
         println(s"Match status = $matchStatus")
        }
        assertEquals(matchStatus.matches, expected, s"Value of matches != expected. Expected: $expected. MatchStatus=${matchStatus})")
      }
     )  
    }
}

