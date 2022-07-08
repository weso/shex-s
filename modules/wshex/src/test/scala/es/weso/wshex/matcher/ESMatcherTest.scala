package es.weso.wshex.matcher
import munit._
import es.weso.wshex.WShExFormat._

/** Test matcher using Entity Schemas as input
  */
class ESMatcherTest extends FunSuite {

  test("Q42_simple".only) {
    val q42Str =
      """|{"type":"item","aliases":{},"labels":{},"descriptions":{},"sitelinks":{},"id":"Q42","claims":{"P31":[{"rank":"normal","references":[{"snaks":{"P214":[{"snaktype":"value","property":"P214","datavalue":{"type":"string","value":"113230702"}}],"P248":[{"snaktype":"value","property":"P248","datavalue":{"type":"wikibase-entityid","value":{"entity-type":"item","numeric-id":54919}},"datatype":"wikibase-item"}],"P813":[{"snaktype":"value","property":"P813","datavalue":{"type":"time","value":{"time":"+00000002013-12-07T00:00:00Z","timezone":0,"before":0,"after":0,"precision":11,"calendarmodel":"http://www.wikidata.org/entity/Q1985727"}},"datatype":"time"}]},"allSnaks":[{"property":"P248","datavalue":{"type":"wikibase-entityid","value":{"entity-type":"item","numeric-id":54919}},"datatype":"wikibase-item"},{"property":"P214","datavalue":{"type":"string","value":"113230702"}},{"property":"P813","datavalue":{"type":"time","value":{"time":"+00000002013-12-07T00:00:00Z","timezone":0,"before":0,"after":0,"precision":11,"calendarmodel":"http://www.wikidata.org/entity/Q1985727"}},"datatype":"time"}],"snaks-order":["P248","P214","P813"]}],"mainsnak":{"snaktype":"value","property":"P31","datavalue":{"type":"wikibase-entityid","value":{"entity-type":"item","numeric-id":5}},"datatype":"wikibase-item"},"id":"Q42$F078E5B3-F9A8-480E-B7AC-D97778CBBEF9","type":"statement"}]}}""".stripMargin
    val schemaStr = """|prefix wd:  <http://www.wikidata.org/entity/>
                       |prefix wdt: <http://www.wikidata.org/prop/direct/>
                       |
                       |start = @<Human>
                       |
                       |<Human> {
                       |  wdt:P31 [ wd:Q5 ]
                       |}""".stripMargin

    val eitherMatcher = Matcher.unsafeFromString(str = schemaStr, format = ESCompactFormat)
    eitherMatcher.fold(
      parseError => fail(s"Error parsing schema: $parseError"),
      matcher => {
        val matchStatus = matcher.matchJsonStart(q42Str)
        assert(
          matchStatus.matches, {
            val wschemaStr = """|prefix : <http://www.wikidata.org/entity/>
                       |
                       |start = @<Human>
                       |
                       |<Human> {
                       |  :P31 [ :Q5 ]
                       |}""".stripMargin
            Matcher
              .unsafeFromString(str = wschemaStr)
              .fold(
                e => s"Cannnot parse wshex matcher",
                matcher2 =>
                  s"Doesn't match Q42\nMatchStatus=${matchStatus})\nMatcher1: ${matcher.wShEx}\nMatcher2: ${matcher2.wShEx}"
              )
          }
        )
      }
    )
  }

  test("Q42_simple with an extra passes") {
    val q42Str =
      """|{"type":"item","aliases":{},"labels":{},"descriptions":{},"sitelinks":{},"id":"Q42","claims":{"P31":[{"rank":"normal","references":[{"snaks":{"P214":[{"snaktype":"value","property":"P214","datavalue":{"type":"string","value":"113230702"}}],"P248":[{"snaktype":"value","property":"P248","datavalue":{"type":"wikibase-entityid","value":{"entity-type":"item","numeric-id":54919}},"datatype":"wikibase-item"}],"P813":[{"snaktype":"value","property":"P813","datavalue":{"type":"time","value":{"time":"+00000002013-12-07T00:00:00Z","timezone":0,"before":0,"after":0,"precision":11,"calendarmodel":"http://www.wikidata.org/entity/Q1985727"}},"datatype":"time"}]},"allSnaks":[{"property":"P248","datavalue":{"type":"wikibase-entityid","value":{"entity-type":"item","numeric-id":54919}},"datatype":"wikibase-item"},{"property":"P214","datavalue":{"type":"string","value":"113230702"}},{"property":"P813","datavalue":{"type":"time","value":{"time":"+00000002013-12-07T00:00:00Z","timezone":0,"before":0,"after":0,"precision":11,"calendarmodel":"http://www.wikidata.org/entity/Q1985727"}},"datatype":"time"}],"snaks-order":["P248","P214","P813"]}],"mainsnak":{"snaktype":"value","property":"P31","datavalue":{"type":"wikibase-entityid","value":{"entity-type":"item","numeric-id":5}},"datatype":"wikibase-item"},"id":"Q42$F078E5B3-F9A8-480E-B7AC-D97778CBBEF9","type":"statement"}]}}""".stripMargin
    val schemaStr = """|prefix wd:  <http://www.wikidata.org/entity/>
                       |prefix wdt: <http://www.wikidata.org/prop/direct/>
                       |
                       |start = @<Human>
                       |
                       |<Human> EXTRA wd:P31 {
                       |  wdt:P31 [ wd:Q5 ]
                       |}""".stripMargin
    val eitherMatcher = Matcher.unsafeFromString(schemaStr)
    eitherMatcher.fold(
      parseError => fail(s"Error parsing schema: $parseError"),
      matcher => {
        val matchStatus = matcher.matchJsonStart(q42Str)
        assert(matchStatus.matches, s"Doesn't match Q42\nMatchStatus=${matchStatus})")
      }
    )
  }

}
