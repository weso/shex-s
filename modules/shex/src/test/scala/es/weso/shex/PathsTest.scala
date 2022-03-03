package es.weso.shex

import es.weso.rdf.nodes._
import cats.data._ 
import cats.effect._
import cats.implicits._
import munit._

class PathsTest extends CatsEffectSuite {


  {
      val shexStr =
        """
          |prefix : <http://example.org/>
          |:A { $<lbl> (:p .; :q .) }
          |:B { :r . ; &<lbl> }
          |""".stripMargin

      val ex = IRI("http://example.org/")
      val p = Direct(ex + "p")
      val q = Direct(ex + "q")
      val r = Direct(ex + "r")
      val a = ex + "A"
      val b = ex + "B"
      shouldMatchPaths(shexStr,a,Set(p,q))
      shouldMatchPaths(shexStr,b,Set(p,q,r))
  }

  {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { $<lbl> (&<lbl>; :p .) }
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val p = Direct(ex + "p")
    val a = ex + "A"
    shouldMatchPaths(shexStr,a,Set(p))
  }

  {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { :p . ; :q . }
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val p = Direct(ex + "p")
    val q = Direct(ex + "q")
    val a = ex + "A"
    shouldMatchPaths(shexStr,a,Set(p,q))
  }

  {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { :p . ; :q . }
        |:B extends @:A { :r . }
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val p = Direct(ex + "p")
    val q = Direct(ex + "q")
    val r = Direct(ex + "r")
    val b = ex + "B"
    shouldMatchPaths(shexStr,b,Set(r))
  }

  {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { :p . ; :q . }
        |:B @:A AND { :r . }
        |:C extends @:B { :s . }
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val p = Direct(ex + "p")
    val q = Direct(ex + "q")
    val r = Direct(ex + "r")
    val s = Direct(ex + "s")
    val c = ex + "C"
    shouldMatchPaths(shexStr,c,Set(s))
  }

  def shouldMatchPaths(strSchema: String, shapeLabel: IRI, paths: Set[Path])(implicit loc: munit.Location): Unit = {
    test(s"Should match paths for schema: ${strSchema} and shapeLabel ${shapeLabel}\nExpected: ${paths}") {
      val shapeLbl = IRILabel(shapeLabel)
      val result = for {
        schema <- EitherT.liftF(Schema.fromString(strSchema))
        shape <- EitherT.fromEither[IO](schema.getShape(shapeLbl))
        paths <- EitherT.fromEither[IO](shape.paths(schema))
      } yield paths
      assertIO(result.value, paths.asRight[String])
    }    
  }

}
