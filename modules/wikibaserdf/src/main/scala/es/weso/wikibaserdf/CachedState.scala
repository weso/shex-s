package es.weso.wikibaserdf
import es.weso.rdf.nodes._
import org.apache.jena.rdf.model.{RDFNode => JenaRDFNode}
import es.weso.rdf.jena.RDFAsJenaModel
import cats.effect._

case class CachedState(iris: Set[IRI], rdf: RDFAsJenaModel)

object CachedState {

  def initial: IO[Resource[IO, CachedState]] = for {
    res <- RDFAsJenaModel.empty
  } yield res.evalMap(rdf =>
    for {
      modelRef <- rdf.getModel
      _ <- IO {
        pprint.pprintln(
          s"CachedState.initial: Model: ${System.identityHashCode(modelRef)}, closed?: ${modelRef.isClosed()}}"
        )
      }
    } yield CachedState(Set(), rdf)
  )
}
