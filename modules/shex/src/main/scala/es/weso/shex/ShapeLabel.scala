package es.weso.shex
import es.weso.shex.shexR.PREFIXES.sx_start
import es.weso.rdf.nodes.{BNode, IRI, RDFNode}
import es.weso.shapemaps.{
  BNodeLabel => BNodeMapLabel,
  IRILabel => IRIMapLabel,
  Start => StartMapLabel,
  _
}
import es.weso.rdf.PrefixMap

abstract sealed trait ShapeLabel extends Product with Serializable {

  def toRDFNode: RDFNode = this match {
    case l: IRILabel   => l.iri
    case l: BNodeLabel => l.bnode
    case Start         => sx_start
  }

  def relativize(base: IRI): ShapeLabel = this match {
    case l: IRILabel => IRILabel(l.iri.relativizeIRI(base))
    case other       => other
  }

  def showQualify(pm: PrefixMap): String = this match {
    case Start             => s"Start"
    case IRILabel(iri)     => pm.qualifyIRI(iri)
    case BNodeLabel(bnode) => bnode.toString
  }

}

case object Start extends ShapeLabel
case class IRILabel(iri: IRI) extends ShapeLabel
case class BNodeLabel(bnode: BNode) extends ShapeLabel

object ShapeLabel {
  def fromRDFNode(node: RDFNode): Either[String, ShapeLabel] = node match {
    case iri: IRI  => Right(IRILabel(iri))
    case bn: BNode => Right(BNodeLabel(bn))
    case _         => Left(s"Cannot convert $node to ShapeLabel")
  }

  def fromString(str: String): Either[String, ShapeLabel] = for {
    node <- RDFNode.fromString(str)
    label <- fromRDFNode(node)
  } yield label

  def fromShapeMapLabel(sml: ShapeMapLabel): ShapeLabel = sml match {
    case StartMapLabel    => Start
    case IRIMapLabel(iri) => IRILabel(iri)
    case BNodeMapLabel(b) => BNodeLabel(b)
  }

}
