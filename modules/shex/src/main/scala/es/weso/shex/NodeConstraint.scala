package es.weso.shex

//import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
// import es.weso.shex.extend.Extend
// import es.weso.shex.normalized.{FlatShape, NormalizedShape}
// import es.weso.utils.EitherUtils._
import es.weso.utils.OptionListUtils._
// import cats._
// import cats.data._
// import cats.implicits._

/*case class NodeConstraint(
    id: Option[ShapeLabel],
    nodeKind: Option[NodeKind],
    datatype: Option[IRI],
    xsFacets: List[XsFacet],
    values: Option[List[ValueSetValue]],
    annotations: Option[List[Annotation]],
    actions: Option[List[SemAct]]
) extends ShapeExpr {
  override def addId(lbl: ShapeLabel): NodeConstraint = {
    this.copy(id = Some(lbl))
  }

  override def paths(schema: AbstractSchema): Either[String, Set[Path]] = Right(Set())

  override def addAnnotations(as: List[Annotation]): NodeConstraint = {
    this.copy(annotations = maybeAddList(annotations, as))
  }
  override def addSemActs(as: List[SemAct]): NodeConstraint = {
    this.copy(actions = maybeAddList(actions, as))
  }

  override def relativize(base: IRI): NodeConstraint =
    NodeConstraint(
      id.map(_.relativize(base)),
      nodeKind,
      datatype.map(_.relativizeIRI(base)),
      xsFacets,
      values.map(_.map(_.relativize(base))),
      annotations.map(_.map(_.relativize(base))),
      actions.map(_.map(_.relativize(base)))
    )

}

object NodeConstraint {

  def empty = NodeConstraint(
    id = None,
    nodeKind = None,
    datatype = None,
    xsFacets = List(),
    values = None,
    annotations = None,
    actions = None
  )

  def nodeKind(nk: NodeKind, facets: List[XsFacet]): NodeConstraint =
    NodeConstraint.empty.copy(nodeKind = Some(nk), xsFacets = facets)

  def nodeKind(idLabel: Option[ShapeLabel], nk: NodeKind, facets: List[XsFacet]): NodeConstraint =
    NodeConstraint.empty.copy(id = idLabel, nodeKind = Some(nk), xsFacets = facets)

  def datatype(dt: IRI, facets: List[XsFacet]): NodeConstraint =
    NodeConstraint.empty.copy(datatype = Some(dt), xsFacets = facets)

  def valueSet(vs: List[ValueSetValue], facets: List[XsFacet]): NodeConstraint =
    NodeConstraint.empty.copy(values = Some(vs), xsFacets = facets)

  def xsFacets(facets: List[XsFacet]): NodeConstraint =
    NodeConstraint.empty.copy(xsFacets = facets)

  def iri: NodeConstraint =
    NodeConstraint.empty.copy(nodeKind = Some(IRIKind))

  def literal: NodeConstraint =
    NodeConstraint.empty.copy(nodeKind = Some(LiteralKind))

  def bNode: NodeConstraint =
    NodeConstraint.empty.copy(nodeKind = Some(BNodeKind))

  def nonLiteral: NodeConstraint =
    NodeConstraint.empty.copy(nodeKind = Some(NonLiteralKind))

}

*/