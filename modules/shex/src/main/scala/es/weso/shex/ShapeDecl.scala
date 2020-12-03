package es.weso.shex

import es.weso.rdf.nodes.IRI

/*case class ShapeDecl(
    id: Option[ShapeLabel],
    _abstract: Boolean,
    shapeExpr: ShapeExpr,
) extends ShapeExpr {

  def addId(lbl: ShapeLabel): ShapeDecl = this.copy(id = Some(lbl))

  def isVirtual: Boolean =
    _abstract

  override def paths(schema: AbstractSchema): Either[String, Set[Path]] = {
    shapeExpr.paths(schema)
  }


  override def addAnnotations(as: List[Annotation]): ShapeExpr = {
    this.copy(shapeExpr = shapeExpr.addAnnotations(as))
  }
  override def addSemActs(as: List[SemAct]): ShapeExpr = {
    this.copy(shapeExpr = shapeExpr.addSemActs(as))
  }

  override def relativize(base: IRI): ShapeDecl =
    this.copy(
        id = id.map(_.relativize(base)),
        shapeExpr = shapeExpr.relativize(base)
    )

}

object ShapeDecl {
  def empty: ShapeDecl = ShapeDecl(
    id = None,
    _abstract = defaultAbstract,
    shapeExpr = Shape.empty
  )

  def defaultAbstract: Boolean = false
}

*/