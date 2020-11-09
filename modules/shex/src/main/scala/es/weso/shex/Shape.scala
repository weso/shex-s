package es.weso.shex

//import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
import es.weso.shex.extend.Extend
import es.weso.shex.normalized.{FlatShape, NormalizedShape}
//import es.weso.utils.EitherUtils._
import es.weso.utils.OptionListUtils._
// import cats._
// import cats.data._
// import cats.implicits._

case class Shape(
    id: Option[ShapeLabel],
    virtual: Option[Boolean],
    closed: Option[Boolean],
    extra: Option[List[IRI]], // TODO: Extend extras to handle Paths?
    expression: Option[TripleExpr],
    _extends: Option[List[ShapeLabel]],
    annotations: Option[List[Annotation]],
    actions: Option[List[SemAct]]
) extends ShapeExpr
    with Extend {

  def normalized(schema: AbstractSchema): Either[String, NormalizedShape] =
    NormalizedShape.fromShape(this, schema)

  def isNormalized(schema: AbstractSchema): Boolean = normalized(schema).isRight

  def isFlatShape(schema: AbstractSchema): Boolean =
    FlatShape.fromShape(this, schema).isRight

  /**
   * If the shape can be flatten, returns a FlatShape
   * 
   * */  
  def flattenShape(schema: AbstractSchema): Either[String, FlatShape] =
    FlatShape.fromShape(this, schema)

  def hasRepeatedProperties(schema: AbstractSchema): Boolean = !isNormalized(schema)

  def addId(lbl: ShapeLabel): Shape = this.copy(id = Some(lbl))

  def isVirtual: Boolean =
    virtual.getOrElse(Shape.defaultVirtual)

  def isClosed: Boolean =
    closed.getOrElse(Shape.defaultClosed)

  // Converts IRIs to direct paths
  def extraPaths: List[Direct] =
    extra.getOrElse(List()).map(Direct)

  def getExtra: List[IRI]              = extra.getOrElse(Shape.emptyExtra)
  def getExtend: List[ShapeLabel]      = _extends.getOrElse(Shape.emptyExtends)
  def getAnnotations: List[Annotation] = annotations.getOrElse(Shape.emptyAnnotations)
  def getActions: List[SemAct]         = actions.getOrElse(Shape.emptySemActs)

  def isEmpty: Boolean = {
    this.id.isEmpty &&
    this.isVirtual == Shape.defaultVirtual &&
    this.isClosed == Shape.defaultClosed &&
    getExtra == Shape.emptyExtra &&
    getExtend == Shape.emptyExtends &&
    getAnnotations == Shape.emptyAnnotations &&
    getActions == Shape.emptySemActs &&
    expression.isEmpty
  }

  private def extend(s: ShapeExpr): Option[List[ShapeLabel]] = s match {
    case s: Shape => s._extends
    case _        => None
  }

  private def expr(s: ShapeExpr): Option[TripleExpr] = s match {
    case s: Shape => s.expression
    case _        => None
  }

  /**
    * Return the paths that are mentioned in a shape
    * @param schema Schema to which the shape belongs, it is needed to resolve references to other shapes
    * @return Set of paths or error in case the shape is not well defined (may have bad references)
    */
  override def paths(schema: AbstractSchema): Either[String, Set[Path]] = {
    def getPath(s: ShapeExpr): Option[List[Path]] = s match {
      case s: Shape => Some(s.expression.map(_.paths(schema).toList).getOrElse(List()))
      case _        => Some(List())
    }
    def combinePaths(p1: List[Path], p2: List[Path]): List[Path] = p1 ++ p2

    extendCheckingVisited(this, schema.getShape(_), extend, combinePaths, getPath).map(_.getOrElse(List())).map(_.toSet)
  }

  def extendExpression(schema: Schema): Either[String, Option[TripleExpr]] = {
    def combine(e1: TripleExpr, e2: TripleExpr): TripleExpr = {
      EachOf(None, List(e1, e2), None, None, None, None)
    }
    extendCheckingVisited(this, schema.getShape(_), extend, combine, expr)
  }

  override def addAnnotations(as: List[Annotation]): ShapeExpr = {
    this.copy(annotations = maybeAddList(annotations, as))
  }
  override def addSemActs(as: List[SemAct]): ShapeExpr = {
    this.copy(actions = maybeAddList(actions, as))
  }

  override def relativize(base: IRI): Shape =
    Shape(
      id.map(_.relativize(base)),
      virtual,
      closed,
      extra.map(_.map(_.relativizeIRI(base))),
      expression.map(_.relativize(base)),
      _extends.map(_.map(_.relativize(base))),
      annotations.map(_.map(_.relativize(base))),
      actions.map(_.map(_.relativize(base)))
    )

   def getExtendsSorted: List[ShapeExpr] = ???

}

object Shape {
  def empty: Shape = Shape(
    id = None,
    virtual = None,
    closed = None,
    extra = None,
    expression = None,
    _extends = None,
    actions = None,
    annotations = None
  )

  def defaultVirtual                     = false
  def defaultClosed                      = false
  def emptyExtra: List[IRI]              = List[IRI]()
  def emptyExtends: List[ShapeLabel]     = List[ShapeLabel]()
  def emptySemActs: List[SemAct]         = List[SemAct]()
  def emptyAnnotations: List[Annotation] = List[Annotation]()
  def defaultExpr: None.type             = None

  def expr(te: TripleExpr): Shape = {
    Shape.empty.copy(expression = Some(te))
  }
}

