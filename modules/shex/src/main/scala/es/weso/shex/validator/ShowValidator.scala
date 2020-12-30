package es.weso.shex.validator
import es.weso.rdf.nodes._
import cats._
import implicits._
import es.weso.shex._
import es.weso.rdf.triples.RDFTriple

class ShowValidator(schema: AbstractSchema) {

  implicit lazy val showIRI = new Show[IRI] {
    override def show(i: IRI): String = {
      schema.qualify(i)
    }
  }

  implicit lazy val showRDFNode = new Show[RDFNode] {
    override def show(n: RDFNode): String = {
      n match {
        case i: IRI => i.show
        case l: Literal => l.getLexicalForm
        case b: BNode => "_:" + b.getLexicalForm
      }
    }
  }

  implicit lazy val showRDFTriple = new Show[RDFTriple] {
    override def show(n: RDFTriple): String = {
      s"<${n.subj.show},${n.pred.show},${n.obj.show}>"
    }
  }

  implicit lazy val showShapeLabel = new Show[ShapeLabel] {
    override def show(lbl: ShapeLabel): String = {
      lbl match {
        case l: IRILabel => Show[RDFNode].show(l.iri)
        case l: BNodeLabel => Show[RDFNode].show(l.bnode)
        case Start => "Start"
      }
    }
  }

  implicit lazy val showPath = new Show[Path] {
    override def show(p: Path): String = {
      p match {
        case Direct(iri) => schema.qualify(iri)
        case Inverse(iri) => "^" + schema.qualify(iri)
      }
    }
  }

  implicit lazy val showAttempt = new Show[Attempt] {
    override def show(a: Attempt): String = {
      val showPath: String = a.path match {
        case None => ""
        case Some(p) => ", path: " + p.show
      }
      s"Attempt: node: ${a.node.show}, shape: ${a.shape.show}${showPath}"
    }
  }

}