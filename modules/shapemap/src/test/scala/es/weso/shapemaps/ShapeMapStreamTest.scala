package es.weso.shapemaps

import munit._
import es.weso.rdf.nodes._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf._
import es.weso.rdf.path.PredicatePath
import cats.effect.IO
import cats.data._
import cats.implicits._
import es.weso.utils.IOUtils.fromES
import fs2._

class ShapeMapStreamTest extends CatsEffectSuite {
  val base = IRI("http://example.org/")

  val chars = List('a')
  def iris = Stream.emits(chars).map(c => base + c.toString)

  def nodes: Stream[IO,RDFNodeSelector] = 
    iris.map(iri => RDFNodeSelector(iri))
  def shapeLabels: Stream[IO,ShapeMapLabel] = 
    iris.map(IRILabel(_))
  def associations: Stream[IO,Association] = 
    (nodes zip shapeLabels).map { 
      case (n,lbl) => Association(n,lbl)
    }

  def validateStream(inputStream:Stream[IO,Association]): Stream[IO,Association] = ???

  test("First test") {

  associations.compile.toList.map(ls => 
   assertEquals(ls, 
    List(
      Association(RDFNodeSelector(base + "a"), IRILabel(base + "a"))), 
      Association(RDFNodeSelector(base + "b"), IRILabel(base + "b")))
    )
      
  }

}
