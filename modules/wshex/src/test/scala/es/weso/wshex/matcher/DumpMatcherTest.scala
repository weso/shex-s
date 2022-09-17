package es.weso.wshex.matcher
import es.weso.utils.FileUtils._
import munit._
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import es.weso.wbmodel.DumpReader
import es.weso.wbmodel.EntityDoc
import cats.effect.IO
import cats._
import cats.implicits._
import java.io.InputStream
import es.weso.wshex.WShExFormat
import es.weso.utils.VerboseLevel
import es.weso.wbmodel.Value
import org.wikidata.wdtk.datamodel.helpers.StatementBuilder
import org.wikidata.wdtk.datamodel.implementation.PropertyIdValueImpl
import org.wikidata.wdtk.datamodel.implementation.ItemIdValueImpl
import org.wikidata.wdtk.datamodel.interfaces.Reference
import org.wikidata.wdtk.datamodel.implementation.ReferenceImpl
import org.wikidata.wdtk.datamodel.helpers.ReferenceBuilder

case class Entities(es: List[EntityDoc]) extends AnyVal
object Entities {
  implicit val EntitiesMonoid: Monoid[Entities] = new Monoid[Entities] {
    override def empty: Entities = Entities(List())
    override def combine(c1: Entities, c2: Entities): Entities =
      Entities(Monoid[List[EntityDoc]].combine(c1.es, c2.es))
  }
}

class DumpMatcherTest extends CatsEffectSuite {

  val humansDumpFile = "humans.json.gz"
  val humansShExFile = "humans.shex"
  val musiciansShExFile = "musicians.shex"

  def getResourceInputStream(fileName: String): InputStream =
    getClass().getClassLoader().getResourceAsStream(fileName)

  def getResourcePath(fileName: String): Path =
    Paths.get(getClass().getClassLoader().getResource(fileName).toURI().getPath())

  {

    val neil: EntityDoc = {
      val ed = EntityDoc.QId(633)
      val p31 = new PropertyIdValueImpl("P31", "http://www.wikidata.org/entity/")
      val p143 = new PropertyIdValueImpl("P143", "http://www.wikidata.org/entity/")
      val p106 = new PropertyIdValueImpl("P106", "http://www.wikidata.org/entity/")
      val q639669 = new ItemIdValueImpl("Q639669", "http://www.wikidata.org/entity/")
      val q5 = new ItemIdValueImpl("Q5", "http://www.wikidata.org/entity/")
      val q48952 = new ItemIdValueImpl("Q48952", "http://www.wikidata.org/entity/")
      val edValue = ed.entityDocument.getEntityId()
      val s1 = StatementBuilder.forSubjectAndProperty(edValue, p31).withValue(q5).build()
      val ref1 = ReferenceBuilder.newInstance().withPropertyValue(p143, q48952).build()
      val s2 = StatementBuilder
        .forSubjectAndProperty(edValue, p106)
        .withValue(q639669)
        .withReference(ref1)
        .withId("Q633$D1BFFBD6-E8B6-45DC-8CC6-154F8D0AD815")
        .build()
      ed.mergeStatements(List(s2, s1))
    }

    checkMatch(
      "Musicians",
      musiciansShExFile,
      humansDumpFile,
      Entities(List(neil)),
      WShExFormat.ESCompactFormat
    )
  }

  private def entityMatch(matcher: Matcher)(e: EntityDoc): IO[Entities] =
    // IO.print(s"Trying to match...${e.getID()}...") >>
    matcher.matchStart(e.entityDocument) match {
      case nm: NoMatching =>
        if (e.getID() == "Q633")
          IO.println(
            s"Error with Neil\n${nm.matchingErrors.map(_.toString.take(300)).mkString("\n")}"
          ) >>
            Entities(List()).pure[IO]
        else
          IO.println(s"No matching for ${e.getID()}") >>
            Entities(List()).pure[IO]
      case m: Matching =>
        IO.println(s"Matches: ${m.entity}!") >>
          Entities(List(m.entity)).pure[IO]
    }

  def checkMatch(
      name: String,
      schemaFile: String,
      dumpFileName: String,
      expected: Entities,
      format: WShExFormat = WShExFormat.ESCompactFormat,
      verboseLevel: VerboseLevel = VerboseLevel.Nothing
  )(implicit loc: munit.Location): Unit =
    test(name) {
      assertIO(
        Matcher
          .fromPath(
            schemaPath = getResourcePath(schemaFile), 
            format = format, 
            verbose = verboseLevel)
          .flatMap(matcher =>
            DumpReader.read(getResourceInputStream(dumpFileName), entityMatch(matcher))
          ),
        expected
      )
    }
}
