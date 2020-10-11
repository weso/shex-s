package es.weso.shex

// import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
// import es.weso.shex.extend.Extend
// import es.weso.shex.normalized.{FlatShape, NormalizedShape}
// import es.weso.utils.EitherUtils._
import es.weso.utils.OptionListUtils._
// import cats._
// import cats.data._
// import cats.implicits._

case class ShapeExternal(id: Option[ShapeLabel], annotations: Option[List[Annotation]], actions: Option[List[SemAct]])
    extends ShapeExpr {
  def addId(lbl: ShapeLabel)                                     = this.copy(id = Some(lbl))
  override def paths(schema: AbstractSchema): Either[String, Set[Path]] = Right(Set())

  override def addAnnotations(as: List[Annotation]): ShapeExpr = {
    this.copy(annotations = maybeAddList(annotations, as))
  }
  override def addSemActs(as: List[SemAct]): ShapeExpr = {
    this.copy(actions = maybeAddList(actions, as))
  }

  override def relativize(base: IRI): ShapeExternal =
    ShapeExternal(
      id.map(_.relativize(base)),
      annotations.map(_.map(_.relativize(base))),
      actions.map(_.map(_.relativize(base)))
    )

}

object ShapeExternal {
  def empty: ShapeExternal = ShapeExternal(None, None, None)
}
