package es.weso.shex

import es.weso.rdf.nodes._
import cats.data._
import cats.effect._
import cats.implicits._
import cats._
import munit._

class PathsTest extends CatsEffectSuite with AllPaths {

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
    shouldMatchPaths(shexStr, a, Set(p, q))
    shouldMatchPaths(shexStr, b, Set(p, q, r))
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
    shouldMatchPaths(shexStr, a, Set(p))
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
    shouldMatchPaths(shexStr, a, Set(p, q))
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
    shouldMatchPaths(shexStr, b, Set(r))
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
    shouldMatchPaths(shexStr, c, Set(s))
  }




  def shouldMatchPaths(strSchema: String, shapeLabel: IRI, paths: Set[Path])(implicit
      loc: munit.Location
  ): Unit =
    test(
      s"Should match paths for schema: ${strSchema} and shapeLabel ${shapeLabel}\nExpected: ${paths}"
    ) {
      val shapeLbl = IRILabel(shapeLabel)
      val result = for {
        schema <- EitherT.liftF(Schema.fromString(strSchema))
        shape <- EitherT.fromEither[IO](schema.getShape(shapeLbl))
        paths <- EitherT.fromEither[IO](shape.paths(schema))
      } yield paths
      assertIO(result.value, paths.asRight[String])
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
    shouldMatchAllPaths(shexStr, c, Set(s, p, q, r))
  }

  {
    val shex =
      """|PREFIX : <http://e/>
         |
         |:Observation CLOSED {
         |  :code . *
         |}
         |
         |# -- super-classes --
         |ABSTRACT :Vital EXTENDS @:Observation { }
         |ABSTRACT :PostureVital EXTENDS @:Vital EXTENDS @:Posture { }
         |
         |# -- BP --
         |:BP EXTENDS @:Vital CLOSED { } 
         |
         |:PostureBP EXTENDS @:BP EXTENDS @:PostureVital { }
         |
         |# -- postures --
         |:Posture {
         |  :code ["posture"]
         |}
         |""".stripMargin
    val ex = IRI("http://e/")
    val code = Direct(ex + "code")
    val se = ex + "PostureBP"
    shouldMatchAllPaths(shex, se, Set(code))         
  }

  {
    val shex =
      """|PREFIX : <http://e/>
         |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
         |
         |:Observation {
         |  :code . * ;
         |}
         |
         |ABSTRACT :Vital EXTENDS @:Observation { }
         |ABSTRACT :PostureVital EXTENDS @:Vital EXTENDS @:Posture { }
         |ABSTRACT :ReclinedVital EXTENDS @:PostureVital EXTENDS @:Reclined { }
         |
         |:BP EXTENDS @:Vital CLOSED { } AND {
         |  :code ["bp"] 
         |}
         |
         |:PostureBP EXTENDS @:BP EXTENDS @:PostureVital  { }
         |:ReclinedBP EXTENDS @:BP EXTENDS @:ReclinedVital { }
         |
         |
         |:Posture {
         |  :code ["posture"] 
         |}
         |
         |:Reclined EXTENDS @:Posture { } 
         |
         |""".stripMargin    
    val ex = IRI("http://e/")
    val code = Direct(ex + "code")
    val se = ex + "Reclined"
    shouldMatchAllPaths(shex, se, Set(code))         
  }



  // TODO: Check allPaths of PostureBP  

  def shouldMatchAllPaths(strSchema: String, shapeLabel: IRI, paths: Set[Path])(implicit
      loc: munit.Location
  ): Unit =
    test(
      s"Should match paths for schema: ${strSchema} and shapeLabel ${shapeLabel}\nExpected: ${paths}"
    ) {
      val shapeLbl = IRILabel(shapeLabel)
      val result = for {
        schema <- EitherT.liftF(Schema.fromString(strSchema))
        se <- EitherT.fromEither[IO](schema.getShape(shapeLbl))
        paths <- EitherT.fromEither[IO](allPaths(se, schema))
      } yield paths
      assertIO(result.value, paths.asRight[String])
    }
  

}
