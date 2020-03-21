package es.weso.shex.validator

import es.weso.rdf.nodes._
import es.weso.shex._
import es.weso.shex.validator.Table.CTable
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should._

class CTableTest extends AnyFunSpec with Matchers with EitherValues {
  describe(s"CTable") {

    it(s"Should generate CTable basic") {
      val ex = "http://example.org/"
      val xsd = IRI(s"http://www.w3.org/2001/XMLSchema#")
      val p = IRI(ex) + "p"
      val xsdString = xsd + "string"
      val te1: TripleExpr = TripleConstraint.datatype(p,xsdString, List())
      // val te2: TripleExpr = TripleConstraint.datatype(q,xsdString, List())
      val te : TripleExpr = te1
      val extras: List[IRI] = List()
      val s: ShapeLabel = IRILabel(IRI(ex + "S"))
      val tripleExprMap : Map[ShapeLabel, TripleExpr] = Map(s -> te)
      val c0 = ConstraintRef(0)
      val cs = List(c0)
      shouldMakeCTable(te,extras,tripleExprMap,cs)
   }

    it(s"Should generate CTable") {
      val ex = "http://example.org/"
      val xsd = IRI(s"http://www.w3.org/2001/XMLSchema#")
      val p = IRI(ex) + "p"
      val q = IRI(ex) + "q"
      val xsdString = xsd + "string"
      val te1: TripleExpr = TripleConstraint.datatype(p,xsdString, List())
      val te2: TripleExpr = TripleConstraint.datatype(q,xsdString, List())
      val te : TripleExpr = EachOf(None, List(te1,te2),None,None,None,None)
      val extras: List[IRI] = List()
      val s: ShapeLabel = IRILabel(IRI(ex + "S"))
      val tripleExprMap : Map[ShapeLabel, TripleExpr] = Map(s -> te)
      val c0 = ConstraintRef(0)
      val c1 = ConstraintRef(1)
      val cs = List(c0,c1)

       shouldMakeCTable(te,extras,tripleExprMap,cs)
    } 

    def shouldMakeCTable(te: TripleExpr, extras: List[IRI], teMap: Map[ShapeLabel,TripleExpr], symbols: List[ConstraintRef]): Unit = {
      val maybeTable = CTable.mkTable(te, extras,teMap)
      maybeTable.fold(e => fail(s"Error: $e"), 
       pair => {
         val (ctable, rbe) = pair
         rbe.symbols should contain theSameElementsAs(symbols)
         info(s"${ctable}")
       }
      )
    }
  }
}
