package es.weso.shex

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
import es.weso.shex.extend.Extend
import es.weso.shex.normalized.{FlatShape, NormalizedShape}
import es.weso.utils.EitherUtils._
import es.weso.utils.OptionListUtils._
import cats._
import cats.data._
import cats.implicits._

sealed trait ShapeExpr extends Product with Serializable {
  def id: Option[ShapeLabel]
  def addId(lbl: ShapeLabel): ShapeExpr

  def showPrefixMap(pm: PrefixMap) = {
    import es.weso.shex.compact.CompactShow._
    showShapeExpr(this, pm)
  }

  def paths(schema: AbstractSchema): Either[String, List[Path]]
  def addAnnotations(as: List[Annotation]): ShapeExpr
  def addSemActs(as: List[SemAct]): ShapeExpr

  def relativize(base: IRI): ShapeExpr

  def hasNoReference(schema: AbstractSchema): Boolean = { 
    getShapeRefs(schema).fold(e => false, _.isEmpty)
  }

  def isSimple(schema: AbstractSchema): Boolean = this match {
    case _: Shape          => false
    case _: NodeConstraint => true
    case _: ShapeExternal  => true
    case _: ShapeOr        => false
    case _: ShapeAnd       => false
    case _: ShapeNot       => false
    case _: ShapeRef       => false
  }

  /**
    * Return the labels that are referenced in a shape expression
    * This method can use useful to detect if a shape doesn't refer to non-existing labels
    */
  def getShapeRefs(schema: AbstractSchema): Either[String, List[ShapeLabel]] = {
    type State = List[ShapeExpr]
    type S[A]  = StateT[Id, State, A]
    type E[A]  = EitherT[S, String, A]
    val initialState: State                       = List()
    def getState: E[State]                        = EitherT.liftF(StateT.get)
    def modifyS(f: State => State): S[Unit]       = StateT.modify(f)
    def modify[A](f: State => State): E[Unit]     = EitherT.liftF(modifyS(f))
    def ok[A](x: A): E[A]                         = EitherT.pure(x)
    def empty: List[ShapeLabel]                   = List()
    def fromEither[A](e: Either[String, A]): E[A] = EitherT.fromEither(e)

    def checkShapeExprRefs(se: ShapeExpr): E[List[ShapeLabel]] =
      for {
        s <- getState
        ps <- if (s contains se) ok(empty)
        else
          for {
            _ <- modify(_ :+ se)
            v <- getShapeExprRefsAux(se)
          } yield v
      } yield ps

    def checkLabel(l: ShapeLabel): E[List[ShapeLabel]] =
      for {
        se <- fromEither(schema.getShape(l))
        ls <- checkShapeExprRefs(se)
      } yield l +: ls

    def getShapeExprRefsAux(se: ShapeExpr): E[List[ShapeLabel]] = {
      se match {
        case nk: NodeConstraint => ok(empty)
        case s: Shape =>
          for {
            labelsExpr <- s.expression.fold(ok(empty))(
              te => {
                for {
                  ls <- getTripleExprRefs(te)
                } yield te.id.fold(ls)(_ +: ls)
              }
            )
            labelsExtends <- s._extends.fold(ok(empty))(labels => labels.map(checkLabel(_)).sequence.map(_.flatten))
          } yield labelsExpr ++ labelsExtends

        case sa: ShapeAnd => sa.shapeExprs.map(checkShapeExprRefs(_)).sequence.map(_.flatten)
        case so: ShapeOr  => so.shapeExprs.map(checkShapeExprRefs(_)).sequence.map(_.flatten)
        case sn: ShapeNot => checkShapeExprRefs(sn.shapeExpr)
        case sr: ShapeRef => {
          for {
            se <- fromEither(schema.getShape(sr.reference))
            ps <- checkShapeExprRefs(se)
          } yield sr.reference +: ps
        }
        case se: ShapeExternal => ok(empty)
      }
    }

    def getTripleExprRefs(te: TripleExpr): E[List[ShapeLabel]] = {
      te match {
        case e: EachOf => e.expressions.map(getTripleExprRefs(_)).sequence.map(_.flatten)
        case o: OneOf  => o.expressions.map(getTripleExprRefs(_)).sequence.map(_.flatten)
        case i: Inclusion =>
          for {
            se <- fromEither(schema.getShape(i.include))
            ls <- getShapeExprRefsAux(se)
          } yield i.include +: ls
        case tc: TripleConstraint =>
          tc.valueExpr.fold(ok(empty))(getShapeExprRefsAux(_))
        case e: Expr => ok(empty)
      }
    }

    val (_, ls) = getShapeExprRefsAux(this).value.run(initialState)
    ls
  }
}

object ShapeExpr {
  def any: ShapeExpr = Shape.empty

  def fail: ShapeExpr = NodeConstraint.valueSet(List(), List())

}

case class ShapeOr(
    id: Option[ShapeLabel],
    shapeExprs: List[ShapeExpr],
    annotations: Option[List[Annotation]],
    actions: Option[List[SemAct]]
) extends ShapeExpr {
  def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))

  override def paths(schema: AbstractSchema): Either[String, List[Path]] =
    sequence(shapeExprs.map(_.paths(schema))).map(_.flatten)

  override def addAnnotations(as: List[Annotation]): ShapeExpr = {
    this.copy(annotations = maybeAddList(annotations, as))
  }
  override def addSemActs(as: List[SemAct]): ShapeExpr = {
    this.copy(actions = maybeAddList(actions, as))
  }

  override def relativize(base: IRI): ShapeOr = ShapeOr(
    id.map(_.relativize(base)),
    shapeExprs.map(_.relativize(base)),
    annotations.map(_.map(_.relativize(base))),
    actions.map(_.map(_.relativize(base)))
  )

}

object ShapeOr {
  def fromShapeExprs(ses: List[ShapeExpr]): ShapeOr =
    ShapeOr(None, ses, None, None)
}

case class ShapeAnd(
    id: Option[ShapeLabel],
    shapeExprs: List[ShapeExpr],
    annotations: Option[List[Annotation]],
    actions: Option[List[SemAct]]
) extends ShapeExpr {
  def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))

  override def paths(schema: AbstractSchema): Either[String, List[Path]] =
    sequence(shapeExprs.map(_.paths(schema))).map(_.flatten)

  override def addAnnotations(as: List[Annotation]): ShapeExpr = {
    this.copy(annotations = maybeAddList(annotations, as))
  }
  override def addSemActs(as: List[SemAct]): ShapeExpr = {
    this.copy(actions = maybeAddList(actions, as))
  }

  override def relativize(base: IRI): ShapeAnd = ShapeAnd(
    id.map(_.relativize(base)),
    shapeExprs.map(_.relativize(base)),
    annotations.map(_.map(_.relativize(base))),
    actions.map(_.map(_.relativize(base)))
  )

}

object ShapeAnd {
  def fromShapeExprs(ses: List[ShapeExpr]): ShapeAnd =
    ShapeAnd(None, ses, None, None)
}

case class ShapeNot(
    id: Option[ShapeLabel],
    shapeExpr: ShapeExpr,
    annotations: Option[List[Annotation]],
    actions: Option[List[SemAct]]
) extends ShapeExpr {
  def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))

  override def paths(schema: AbstractSchema): Either[String, List[Path]] =
    shapeExpr.paths(schema)

  override def addAnnotations(as: List[Annotation]): ShapeExpr = {
    this.copy(annotations = maybeAddList(annotations, as))
  }
  override def addSemActs(as: List[SemAct]): ShapeExpr = {
    this.copy(actions = maybeAddList(actions, as))
  }

  override def relativize(base: IRI): ShapeNot = ShapeNot(
    id.map(_.relativize(base)),
    shapeExpr.relativize(base),
    annotations.map(_.map(_.relativize(base))),
    actions.map(_.map(_.relativize(base)))
  )

}

object ShapeNot {
  def fromShapeExpr(se: ShapeExpr): ShapeNot =
    ShapeNot(None, se, None, None)
}

case class NodeConstraint(
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

  override def paths(schema: AbstractSchema): Either[String, List[Path]] = Right(List())

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
    * @return List of paths or error in case the shape is not well defined (may have bad references)
    */
  def paths(schema: AbstractSchema): Either[String, List[Path]] = {
    def getPath(s: ShapeExpr): Option[List[Path]] = s match {
      case s: Shape => Some(s.expression.map(_.paths(schema)).getOrElse(List()))
      case _        => Some(List())
    }
    def combinePaths(p1: List[Path], p2: List[Path]): List[Path] = p1 ++ p2
    extendCheckingVisited(this, schema.getShape(_), extend, combinePaths, getPath).map(_.getOrElse(List()))
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

case class ShapeRef(reference: ShapeLabel, annotations: Option[List[Annotation]], actions: Option[List[SemAct]])
    extends ShapeExpr {
  def id                     = None
  def addId(lbl: ShapeLabel) = this

  override def paths(schema: AbstractSchema): Either[String, List[Path]] =
    for {
      se <- schema.getShape(reference)
      ps <- se.paths(schema)
    } yield ps

  override def addAnnotations(as: List[Annotation]): ShapeExpr = {
    this.copy(annotations = maybeAddList(annotations, as))
  }
  override def addSemActs(as: List[SemAct]): ShapeExpr = {
    this.copy(actions = maybeAddList(actions, as))
  }

  override def relativize(base: IRI): ShapeRef =
    ShapeRef(
      reference.relativize(base),
      annotations.map(_.map(_.relativize(base))),
      actions.map(_.map(_.relativize(base)))
    )

}

case class ShapeExternal(id: Option[ShapeLabel], annotations: Option[List[Annotation]], actions: Option[List[SemAct]])
    extends ShapeExpr {
  def addId(lbl: ShapeLabel)                                     = this.copy(id = Some(lbl))
  override def paths(schema: AbstractSchema): Either[String, List[Path]] = Right(List())

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
