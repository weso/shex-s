package es.weso.shapemaps

import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._
import es.weso.rdf.path.PredicatePath
import cats.effect.IO
import cats.data._
import cats.implicits._
import es.weso.utils.IOUtils.fromES
import munit.CatsEffectSuite


class ShapeMapTest extends CatsEffectSuite {

  test("should be able to create a shape map") {
      val map = QueryShapeMap(
        List(Association(node = RDFNodeSelector(IRI("http://example.org/x")), shape = Start)),
        PrefixMap.empty,
        PrefixMap.empty)
      assertEquals(map.associations.length, 1)
  }

  { // ShapeMaps parser

    val nodesPrefixMap = PrefixMap.empty.
      addPrefix("", IRI("http://default.org/")).
      addPrefix("ex", IRI("http://example.org/"))
    val shapesPrefixMap = PrefixMap.empty.
      addPrefix("", IRI("http://default.shapes.org/")).
      addPrefix("ex", IRI("http://shapes.org/"))
    val rdfType = IRI("http://www.w3.org/1999/02/22-rdf-syntax-ns#type")

    shouldParse(
      "<http://example.org/x> @ Start",
      QueryShapeMap(
        List(Association(node = RDFNodeSelector(IRI("http://example.org/x")), shape = Start)), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse(
      "23@Start",
      QueryShapeMap(
        List(Association(
          node = RDFNodeSelector(IntegerLiteral(23)), shape = Start)),
        nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse(
      "<http://example.org/x>@Start",
      QueryShapeMap(List(Association(node = RDFNodeSelector(IRI("http://example.org/x")), shape = Start)), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse(
      "<http://example.org/x>@<http://example.org/S>",
      QueryShapeMap(
        List(Association(
          node = RDFNodeSelector(IRI("http://example.org/x")),
          shape = IRILabel(IRI("http://example.org/S")))),
        nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)
    shouldParse(
      ":x@Start",
      QueryShapeMap(List(Association(node = RDFNodeSelector(IRI("http://default.org/x")), shape = Start)), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)
    shouldParse(
      ":x@ :S",
      QueryShapeMap(
        List(
          Association(
            node = RDFNodeSelector(IRI("http://default.org/x")),
            shape = IRILabel(IRI("http://default.shapes.org/S")))), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)
    /*    Ignore this test until we solve issue #48
      https://github.com/labra/shaclex/issues/48

      shouldParse(
      "\"hi\"@es @ :S",
      QueryShapeMap(List(Association(
        node = RDFNodeSelector(LangLiteral("hi", Lang("es"))),
        shape = IRILabel(IRI("http://default.shapes.org/S")))), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap) */
    shouldParse(
      ":x@ ex:S",
      QueryShapeMap(List(Association(
        node = RDFNodeSelector(IRI("http://default.org/x")),
        shape = IRILabel(IRI("http://shapes.org/S")))), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse(
      "{ FOCUS a :A} @ ex:S",
      QueryShapeMap(List(Association(
        node = TriplePattern(Focus, PredicatePath(rdfType), NodePattern(IRI("http://default.org/A"))),
        shape = IRILabel(IRI("http://shapes.org/S")))), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse(
      "{FOCUS :p _ }@ :S",
      QueryShapeMap(List(Association(
        node = TriplePattern(Focus, PredicatePath(IRI("http://default.org/p")), WildCard),
        shape = IRILabel(IRI("http://default.shapes.org/S")))), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse(
      """SPARQL "select ?item where { ?x a ?item }"@:S""",
      QueryShapeMap(List(Association(
        node = SparqlSelector("select ?item where { ?x a ?item }"),
        shape = IRILabel(IRI("http://default.shapes.org/S")))),
        nodesPrefixMap,
        shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    shouldParse("""<http://x.com>@<S>""", QueryShapeMap(List(Association(
      node = RDFNodeSelector(IRI("http://x.com")),
      shape = IRILabel(IRI("S")))), nodesPrefixMap, shapesPrefixMap),
      nodesPrefixMap,
      shapesPrefixMap)

    def shouldParse(
      str: String,
      expected: ShapeMap,
      nodesPrefixMap: PrefixMap,
      shapesPrefixMap: PrefixMap): Unit = {
      test(s"should parse $str and obtain $expected") {
        Parser.parse(str, Some(IRI("")), nodesPrefixMap, shapesPrefixMap) match {
          case Left(msg) => fail(s"Failed to parse $str: $msg")
          case Right(shapeMap) => {
            println(s"$str parsed!!")
            println(shapeMap)
            // info(s"Parsed $shapeMap")
            val sm: ShapeMap = shapeMap
            assertEquals(sm, expected)
          }
        }
      }
    }
  }

  { // Fix shape map

    val rdfStr =
      """
        |prefix : <http://example.org/>
        |:x a :X ;
        |   :p :y, :z .
        |:t a :X .
        |:u :q :w .
        |:v :q :w .
        |:a :p :y .
        |:b :p :y .
        |:c :p :z .
      """.stripMargin

    shouldFixAs(":x@ :S", rdfStr, ":x@ :S")
    shouldFixAs("{FOCUS a :X }@ :S", rdfStr, ":x@ :S, :t @ :S")
    shouldFixAs("{FOCUS :p :y }@ :S", rdfStr, ":x@ :S, :a@ :S, :b@ :S")
    shouldFixAs("{FOCUS :p _ }@ :S", rdfStr, ":x@ :S, :a@ :S, :b @ :S, :c @ :S")
    shouldFixAs("""|SPARQL '''prefix : <http://example.org/>
                   |        select ?item where { ?item a :X }'''@ :S""".stripMargin,
                   rdfStr, ":x@:S,:t@:S"
    )

    def either2IO[A](e: Either[NonEmptyList[String],A]): IO[A] = 
      e.fold(s => IO.raiseError(new RuntimeException(s.toList.mkString("\n"))), IO(_))

    def shouldFixAs(shapeMapStr: String,
                    rdfStr: String,
                    expectedStr: String): Unit = {
      val shapesPrefixMap =
        PrefixMap.empty.addPrefix("", IRI("http://example.org/"))

      test(s"should fix $shapeMapStr and obtain $expectedStr") {
        val v: IO[(ShapeMap,ShapeMap)] = RDFAsJenaModel.fromString(rdfStr,"TURTLE").flatMap(_.use(rdf =>
          for {
          rdfPrefixMap <- rdf.getPrefixMap
          shapeMap <- either2IO(Parser.parse(shapeMapStr, None, rdfPrefixMap, shapesPrefixMap))
          expected <- either2IO(Parser.parse(expectedStr, None, rdfPrefixMap, shapesPrefixMap))
          obtained <- ShapeMap.fixShapeMap(shapeMap, rdf, rdfPrefixMap, shapesPrefixMap)
        } yield (obtained,expected)))
        // val (expected,obtained) = v.unsafeRunSync()
        v.map(pair => {
          val (expected,obtained) = pair
          assertEquals(obtained.associations, expected.associations)
        })
        
        
        /* RDFAsJenaModel.fromChars(rdfStr, "TURTLE") match {
          case Left(e) => fail(s"Error parsing $rdfStr")
          case Right(rdf) => {
            val result = for {
              shapeMap <- Parser.parse(shapeMapStr, None, rdf.getPrefixMap, shapesPrefixMap)
              expected <- Parser.parse(expectedStr, None, rdf.getPrefixMap, shapesPrefixMap)
              obtained <- ShapeMap.fixShapeMap(shapeMap, rdf, rdf.getPrefixMap, shapesPrefixMap)
            } yield (obtained, expected)
            result match {
              case Left(msg) => fail(s"Error $msg fixing map $shapeMapStr")
              case Right((obtained, expected)) =>
                obtained.associations should contain theSameElementsAs (expected.associations)
            }
          }
        }
      } */
    } 
   }
  }

   { // Show and parse shapeMaps
    val rdfStr =
      """
        |prefix : <http://example.org/>
        |
        |:x a :X .
        |
      """.stripMargin
    shouldShowAndParse(":a @ :S", rdfStr)
    shouldShowAndParse("{FOCUS a :X}@ :S", rdfStr)
    shouldShowAndParse("{FOCUS a _}@ :S", rdfStr)
    shouldShowAndParse("{FOCUS :p :X}@ :S", rdfStr)

    def shouldShowAndParse(shapeMapStr: String, rdfStr: String): Unit = {
      val shapesPrefixMap = PrefixMap.empty.addPrefix("", IRI("http://example.org/"))
      test(s"Should show and parse $shapeMapStr") {
        val r = RDFAsJenaModel.fromChars(rdfStr, "TURTLE").flatMap(_.use(rdf => for {
          rdfPrefixMap <- rdf.getPrefixMap
          parsedSm <- fromES(
            Parser.parse(shapeMapStr, None, rdfPrefixMap, shapesPrefixMap).leftMap(err => s"Error parsing: ${shapeMapStr}:$err")
          )
          shownSm = parsedSm.toString
          parsedShown <- fromES(Parser.parse(shownSm, None, rdfPrefixMap, shapesPrefixMap).leftMap(err => s"Error parsing: ${shownSm}:$err"))
        } yield (parsedSm,parsedShown)))
         r.map(pair => {
           val (sm1,sm2) = pair
           assertEquals(sm1, sm2)
           }
         )
      }
    }
  }


}
