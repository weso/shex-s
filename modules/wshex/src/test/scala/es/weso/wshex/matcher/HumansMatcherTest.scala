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

case class Counter(n: Int) extends AnyVal
object Counter {
  implicit val CounterMonoid: Monoid[Counter] = new Monoid[Counter] {
    override def empty: Counter = Counter(0)
    override def combine(c1: Counter, c2: Counter): Counter =
      Counter(c1.n + c2.n)
  }
}

class HumansMatcherTest extends CatsEffectSuite {

  val humansDumpFile = "humans.json.gz"
  val humansShExFile = "humans.shex"
  def humansIS(): InputStream =
    getClass.getClassLoader.getResourceAsStream(humansDumpFile)

  test("Count only test") {
    def counter(e: EntityDoc): IO[Counter] = Counter(1).pure[IO]
    val countAction = DumpReader.read(humansIS(), counter)
    assertIO(countAction, Counter(15))
  }

  test("Counter match human") {
    val schemaStr = """|prefix : <http://www.wikidata.org/entity/>
                       |
                       |start = @<Human>
                       |
                       |<Human> {
                       |  :P31 [ :Q5 ] ; # instance of Human
                       |}""".stripMargin
    val eitherMatcher = Matcher.unsafeFromString(schemaStr)
    eitherMatcher.fold(
      parseError => fail(s"Error parsing schema: $parseError"),
      matcher => {
        def counterMatch(e: EntityDoc): IO[Counter] =
          matcher.matchStart(e.entityDocument) match {
            case _: NoMatching => Counter(0).pure[IO]
            case _: Matching   => Counter(1).pure[IO]
          }
        assertIO(DumpReader.read(humansIS(), counterMatch), Counter(15))
      }
    )
  }

  


}
