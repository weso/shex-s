package es.weso.shex

import es.weso.rdf.nodes._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class PathsTest extends AnyFunSpec with Matchers with EitherValues {

  describe(s"Calculates paths of a shape") {
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
      shouldMatchPaths(shexStr,a,List(p,q))
      shouldMatchPaths(shexStr,b,List(p,q,r))
  }

  describe(s"Calculates paths of a shape with a label that refers to itself") {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { $<lbl> (&<lbl>; :p .) }
        |""".stripMargin

    val ex = IRI("http://example.org/")
    val p = Direct(ex + "p")
    val a = ex + "A"
    shouldMatchPaths(shexStr,a,List(p))
}

  def shouldMatchPaths(strSchema: String, shapeLabel: IRI, paths: List[Path]) = {
    it(s"Should calculate paths of shape $shapeLabel and return $paths") {
      val shapeLbl = IRILabel(shapeLabel)
      val result = for {
        schema <- Schema.fromString(strSchema)
        shape <- schema.getShape(shapeLbl)
        paths <- shape.paths(schema)
      } yield paths
      result.fold(e => fail(s"Error: $e"),
        ps => ps should contain theSameElementsAs (paths)
      )
    }
  }

}
