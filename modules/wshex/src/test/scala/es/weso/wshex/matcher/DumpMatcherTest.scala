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
import es.weso.wbmodel.Entity
import es.weso.utils.VerboseLevel
import es.weso.wbmodel.Value

case class Entities(es: List[Entity]) extends AnyVal
object Entities {
  implicit val EntitiesMonoid: Monoid[Entities] = new Monoid[Entities] {
    override def empty: Entities = Entities(List())
    override def combine(c1: Entities, c2: Entities): Entities =
      Entities(Monoid[List[Entity]].combine(c1.es, c2.es))
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
    val e: Entity = Value.Qid(633)
    checkMatch(
      "Musicians",
      musiciansShExFile,
      humansDumpFile,
      Entities(List(e)),
      WShExFormat.ESCompactFormat
    )
  }

  /*  checkMatch(
    "Humans",
    humansShExFile,
    humansDumpFile,
    Entities(List()),
    WShExFormat.ESCompactFormat
  ) */

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
          .fromPath(getResourcePath(schemaFile), format, verboseLevel)
          .flatMap(matcher =>
            IO.println(s"Matcher obtained...${matcher.wShEx}") >>
              DumpReader.read(getResourceInputStream(dumpFileName), entityMatch(matcher))
          ),
        expected
      )
    }
}
