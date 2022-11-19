package es.weso.wshex.matcher
import munit._
import es.weso.wshex.WShExFormat._
import org.wikidata.wdtk.datamodel.helpers
import es.weso.utils.VerboseLevel
import org.wikidata.wdtk.datamodel.interfaces.{Reference => WDTKReference, _}
import org.wikidata.wdtk.datamodel.helpers._
import org.wikidata.wdtk.datamodel.helpers.ItemDocumentBuilder._
import org.wikidata.wdtk.datamodel.implementation._
import es.weso.wbmodel._
import cats.implicits._

/** Test matcher using Entity Schemas as input
  */
trait CheckMatchWShEx extends FunSuite {

  def checkMatch(
      name: String,
      schemaStr: String,
      ed: EntityDocument,
      expected: Option[EntityDoc],
      verboseLevel: VerboseLevel = VerboseLevel.Nothing
  )(implicit loc: munit.Location): Unit =
    test(name) {
      Matcher
        .unsafeFromString(str = schemaStr, format = CompactWShExFormat, verbose = verboseLevel)
        .fold(
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
              case None =>
                matchStatus match {
                  case nm: NoMatching => assertEquals(true, true)
                  case m: Matching =>
                    fail(s"""|Expected to fail but passed with matching\n$m""".stripMargin)
                }
              case Some(ed) =>
                matchStatus match {
                  case nm: NoMatching => fail(s"""|Expected to pass but failed
                                           |Error: $nm
                                           |Expected: $ed
                                           |""".stripMargin)
                  case m: Matching =>
                    assertEquals(
                      m.entity,
                      ed,
                      s"""|Value of matches != expected
                |Expected: $expected
                |Obtained=${matchStatus}
                |""".stripMargin
                    )
                }

            }
          }
        )
    }
}
