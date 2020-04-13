package es.weso.shex
import org.scalatest._
//import cats.effect.IO
//import cats.data.EitherT
import matchers.should._
import funspec._
import es.weso.utils.eitherios.EitherIOUtils._  
import es.weso.rdf.nodes.IRI

class ShapeExprTest extends AnyFunSpec with Matchers {

  describe(s"Calculate paths") {
    it(s"Calculates paths of a Shape Expr") {
    val ex = "http://example.org/"  
    val str = s"""|prefix : <${ex}>
                  |:S CLOSED EXTRA :p {
                  | :p [ 1 2 ] ;
                  | :p [ 2 3 ]  
                  }""".stripMargin
    val s = IRILabel(IRI(s"${ex}S"))
    val p = Direct(IRI(s"${ex}p"))
    val r = for {
      schema <- Schema.fromString(str)
      se <- eitherStr2IO(schema.getShape(s))
      paths <- eitherStr2IO(se.paths(schema))
    } yield (se,paths,schema)
    r.attempt.unsafeRunSync.fold(e => fail(s"Error: $e"), values => {
      val (_,paths,_) = values
      paths should contain theSameElementsAs(List(p))
    }
    )
  }
 }

}
