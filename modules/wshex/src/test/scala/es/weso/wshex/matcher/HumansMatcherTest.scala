package es.weso.wshex.matcher
import es.weso.utils.FileUtils._
import munit._
import java.nio.file.Path
import java.nio.file.Paths

class HumansMatcherTest extends FunSuite {

  val humansDumpFile = "humans.json.gz"
  val humansShExFile = "humans.shex"
  test("HumansMatcher") {

    for {
      humansStr <- getContents(Paths.get(humansDumpFile))
      entityDoc 
    } yield ss
    val schemaStr = """|prefix : <http://www.wikidata.org/entity/>
                       |
                       |start = @<Human>
                       |
                       |<Volcano> {
                       |  :P31 [ :Q212057 ] ; # instance of Volcano
                       |  :P361 .            # part of 
                       |}""".stripMargin
    val eitherMatcher = Matcher.unsafeFromString(schemaStr)
    eitherMatcher.fold(
      parseError => fail(s"Error parsing schema: $parseError"),
      matcher => {
        val matchStatus = matcher.matchJsonStart(ertaStr)
        assert(matchStatus.matches, s"Doesn't match Q903\nMatchStatus=${matchStatus})")
      }
    )
  }
}
