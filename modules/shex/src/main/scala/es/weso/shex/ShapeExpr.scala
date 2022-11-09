package es.weso.shex

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
import cats._
import cats.data._
import cats.implicits._
import es.weso.utils.EitherUtils._
import es.weso.utils.OptionListUtils._
import es.weso.shex.extend.Extend
import es.weso.shex.normalized.{FlatShape, NormalizedShape}

sealed abstract trait ShapeExpr extends Product with Serializable {
  def id: Option[ShapeLabel]
  def addId(lbl: ShapeLabel): ShapeExpr
  def rmId: ShapeExpr

  def showQualified(pm: PrefixMap) = {
    import es.weso.shex.compact.CompactShow._
    showShapeExpr(this, pm)
  }

  def showId(pm: PrefixMap) =
    id.map(_.showQualify(pm)).getOrElse("?")

  def paths(schema: AbstractSchema): Either[String, Set[Path]]
  def addAnnotations(as: List[Annotation]): ShapeExpr
  def addSemActs(as: List[SemAct]): ShapeExpr

  def relativize(base: IRI): ShapeExpr

  def hasNoReference(schema: AbstractSchema): Boolean =
    getShapeRefs(schema).fold(e => false, _.isEmpty)

  def isSimple: Boolean = this match {
    case s: Shape if s == Shape.empty => true
    case _: Shape                     => false
    case _: NodeConstraint            => true
    case _: ShapeExternal             => true
    case _: ShapeOr                   => false
    case _: ShapeAnd                  => false
    case _: ShapeNot                  => false
    case _: ShapeRef                  => false
    case _: ShapeDecl                 => false
  }

  /** Return the labels that are referenced in a shape expression
    * This method can use useful to detect if a shape doesn't refer to non-existing labels
    */
  def getShapeRefs(schema: AbstractSchema): Either[String, List[ShapeLabel]] = {
    type State = List[ShapeExpr]
    type S[A] = StateT[Id, State, A]
    type E[A] = EitherT[S, String, A]
    val initialState: State = List()
    def getState: E[State] = EitherT.liftF(StateT.get)
    def modifyS(f: State => State): S[Unit] = StateT.modify(f)
    def modify[A](f: State => State): E[Unit] = EitherT.liftF(modifyS(f))
    def ok[A](x: A): E[A] = EitherT.pure(x)
    def empty: List[ShapeLabel] = List()
    def fromEither[A](e: Either[String, A]): E[A] = EitherT.fromEither(e)
    def err[A](msg: String): E[A] = EitherT.leftT(msg)

    def checkShapeExprRefs(se: ShapeExpr): E[List[ShapeLabel]] =
      for {
        s <- getState
        ps <-
          if (s contains se) ok(empty)
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

    def getShapeExprRefsAux(se: ShapeExpr): E[List[ShapeLabel]] =
      se match {
        case nk: NodeConstraint => ok(empty)
        case s: Shape =>
          for {
            labelsExpr <- s.expression.fold(ok(empty)) { te =>
              for {
                ls <- getTripleExprRefs(te)
              } yield te.id.fold(ls)(_ +: ls)
            }
            labelsExtends <- s._extends.fold(ok(empty))(labels =>
              labels.map(checkLabel(_)).sequence.map(_.flatten)
            )
          } yield labelsExpr ++ labelsExtends

        case sa: ShapeAnd => sa.shapeExprs.map(checkShapeExprRefs(_)).sequence.map(_.flatten)
        case so: ShapeOr  => so.shapeExprs.map(checkShapeExprRefs(_)).sequence.map(_.flatten)
        case sn: ShapeNot => checkShapeExprRefs(sn.shapeExpr)
        case sr: ShapeRef =>
          for {
            se <- fromEither(schema.getShape(sr.reference))
            ps <- checkShapeExprRefs(se)
          } yield sr.reference +: ps
        case se: ShapeExternal => ok(empty)
        case sd: ShapeDecl     => getShapeExprRefsAux(sd.shapeExpr)
        // case _ => err(s"getShapeExprRefsAux: Unsupported type of shapeExpr: ${se}")
      }

    def getTripleExprRefs(te: TripleExpr): E[List[ShapeLabel]] =
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

    val (_, ls) = getShapeExprRefsAux(this).value.run(initialState)
    ls
  }

  def children(schema: AbstractSchema): List[Shape] =
    List()

  /*  lazy val flattenTCs: Set[TripleConstraint] = this match {
    case sa: ShapeAnd => {
      val ls = sa.shapeExprs.map(_.flattenTCs)
      ???
    }
  } */
}

case class ShapeOr(
    id: Option[ShapeLabel],
    shapeExprs: List[ShapeExpr],
    annotations: Option[List[Annotation]],
    actions: Option[List[SemAct]]
) extends ShapeExpr {
  override def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))
  override def rmId = this.copy(id = None)

  override def paths(schema: AbstractSchema): Either[String, Set[Path]] =
    sequence(shapeExprs.map(_.paths(schema))).map(_.toSet.flatten)

  override def addAnnotations(as: List[Annotation]): ShapeExpr =
    this.copy(annotations = maybeAddList(annotations, as))
  override def addSemActs(as: List[SemAct]): ShapeExpr =
    this.copy(actions = maybeAddList(actions, as))

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
  override def rmId = this.copy(id = None)

  override def paths(schema: AbstractSchema): Either[String, Set[Path]] =
    sequence(shapeExprs.map(_.paths(schema))).map(_.toSet.flatten)

  override def addAnnotations(as: List[Annotation]): ShapeExpr =
    this.copy(annotations = maybeAddList(annotations, as))
  override def addSemActs(as: List[SemAct]): ShapeExpr =
    this.copy(actions = maybeAddList(actions, as))

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

object ShapeExpr {

  def any: ShapeExpr = Shape.empty

  def fail: ShapeExpr = NodeConstraint.valueSet(List(), List())

  def and(ses: ShapeExpr*): ShapeExpr =
    ShapeAnd(None, ses.toList, None, None)

}

case class Shape(
    id: Option[ShapeLabel],
    virtual: Option[Boolean],
    closed: Option[Boolean],
    extra: Option[List[IRI]], // TODO: Extend extras to handle Paths?
    expression: Option[TripleExpr],
    _extends: Option[List[ShapeLabel]],
    restricts: Option[List[ShapeLabel]],
    annotations: Option[List[Annotation]],
    actions: Option[List[SemAct]]
) extends ShapeExpr
    with Extend {

  def normalized(schema: AbstractSchema): Either[String, NormalizedShape] =
    NormalizedShape.fromShape(this, schema)

  def isNormalized(schema: AbstractSchema): Boolean = normalized(schema).isRight

  def isFlatShape(schema: AbstractSchema): Boolean =
    FlatShape.fromShape(this, schema).isRight

  /** If the shape can be flatten, returns a FlatShape
    */
  def flattenShape(schema: AbstractSchema): Either[String, FlatShape] =
    FlatShape.fromShape(this, schema)

  def hasRepeatedProperties(schema: AbstractSchema): Boolean = !isNormalized(schema)

  override def addId(lbl: ShapeLabel): Shape = this.copy(id = Some(lbl))
  override def rmId = this.copy(id = None)

  def isVirtual: Boolean =
    virtual.getOrElse(Shape.defaultVirtual)

  def isClosed: Boolean =
    closed.getOrElse(Shape.defaultClosed)

  // Converts IRIs to direct paths
  def extraPaths: List[Direct] =
    extra.getOrElse(List()).map(Direct.apply)

  def getExtra: List[IRI] = extra.getOrElse(Shape.emptyExtra)
  def getExtend: List[ShapeLabel] = _extends.getOrElse(Shape.emptyExtends)
  def getAnnotations: List[Annotation] = annotations.getOrElse(Shape.emptyAnnotations)
  def getActions: List[SemAct] = actions.getOrElse(Shape.emptySemActs)

  def isEmpty: Boolean =
    this.id.isEmpty &&
      this.isVirtual == Shape.defaultVirtual &&
      this.isClosed == Shape.defaultClosed &&
      getExtra == Shape.emptyExtra &&
      getExtend == Shape.emptyExtends &&
      getAnnotations == Shape.emptyAnnotations &&
      getActions == Shape.emptySemActs &&
      expression.isEmpty

  override def paths(schema: AbstractSchema): Either[String, Set[Path]] =
    expression.map(_.paths(schema)).getOrElse(Set()).asRight[String]

  def withExtends(es: ShapeLabel*): Shape = this.copy(_extends = es.toList.some)

  def withExpr(te: TripleExpr): Shape = this.copy(expression = te.some)

  override def addAnnotations(as: List[Annotation]): ShapeExpr =
    this.copy(annotations = maybeAddList(annotations, as))
  override def addSemActs(as: List[SemAct]): ShapeExpr =
    this.copy(actions = maybeAddList(actions, as))

  override def relativize(base: IRI): Shape =
    Shape(
      id.map(_.relativize(base)),
      virtual,
      closed,
      extra.map(_.map(_.relativizeIRI(base))),
      expression.map(_.relativize(base)),
      _extends.map(_.map(_.relativize(base))),
      restricts.map(_.map(_.relativize(base))),
      annotations.map(_.map(_.relativize(base))),
      actions.map(_.map(_.relativize(base)))
    )

  def withExpr(te: Option[TripleExpr]): Shape =
    this.copy(expression = te)

  def withExtra(extras: Option[List[IRI]]): Shape =
    this.copy(extra = extras)

}

object Shape {
  def empty: Shape = Shape(
    id = None,
    virtual = None,
    closed = None,
    extra = None,
    expression = None,
    _extends = None,
    restricts = None,
    actions = None,
    annotations = None
  )

  def defaultVirtual = false
  def defaultClosed = false
  def emptyExtra: List[IRI] = List[IRI]()
  def emptyExtends: List[ShapeLabel] = List[ShapeLabel]()
  def emptySemActs: List[SemAct] = List[SemAct]()
  def emptyAnnotations: List[Annotation] = List[Annotation]()
  def defaultExpr: None.type = None

  def expr(te: TripleExpr): Shape =
    Shape.empty.copy(expression = Some(te))
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
  override def addId(lbl: ShapeLabel): NodeConstraint =
    this.copy(id = Some(lbl))
  override def rmId = this.copy(id = None)

  override def paths(schema: AbstractSchema): Either[String, Set[Path]] = Right(Set())

  override def addAnnotations(as: List[Annotation]): NodeConstraint =
    this.copy(annotations = maybeAddList(annotations, as))
  override def addSemActs(as: List[SemAct]): NodeConstraint =
    this.copy(actions = maybeAddList(actions, as))

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

  def withValues(vs: ValueSetValue*): NodeConstraint =
    this.copy(values = vs.toList.some)
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

case class ShapeNot(
    id: Option[ShapeLabel],
    shapeExpr: ShapeExpr,
    annotations: Option[List[Annotation]],
    actions: Option[List[SemAct]]
) extends ShapeExpr {
  override def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))
  override def rmId = this.copy(id = None)

  override def paths(schema: AbstractSchema): Either[String, Set[Path]] =
    shapeExpr.paths(schema)

  override def addAnnotations(as: List[Annotation]): ShapeExpr =
    this.copy(annotations = maybeAddList(annotations, as))
  override def addSemActs(as: List[SemAct]): ShapeExpr =
    this.copy(actions = maybeAddList(actions, as))

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

case class ShapeRef(
    reference: ShapeLabel,
    annotations: Option[List[Annotation]],
    actions: Option[List[SemAct]]
) extends ShapeExpr {
  def id = None
  override def addId(lbl: ShapeLabel) = this
  override def rmId = this

  override def paths(schema: AbstractSchema): Either[String, Set[Path]] =
    for {
      se <- schema.getShape(reference)
      ps <- se.paths(schema)
    } yield ps

  override def addAnnotations(as: List[Annotation]): ShapeExpr =
    this.copy(annotations = maybeAddList(annotations, as))
  override def addSemActs(as: List[SemAct]): ShapeExpr =
    this.copy(actions = maybeAddList(actions, as))

  override def relativize(base: IRI): ShapeRef =
    ShapeRef(
      reference.relativize(base),
      annotations.map(_.map(_.relativize(base))),
      actions.map(_.map(_.relativize(base)))
    )

}

object ShapeRef {
  def apply(iri: IRI): ShapeRef = ShapeRef(IRILabel(iri), None, None)
}

case class ShapeExternal(
    id: Option[ShapeLabel],
    annotations: Option[List[Annotation]],
    actions: Option[List[SemAct]]
) extends ShapeExpr {
  override def addId(lbl: ShapeLabel) = this.copy(id = Some(lbl))
  override def rmId = this.copy(id = None)

  override def paths(schema: AbstractSchema): Either[String, Set[Path]] = Right(Set())

  override def addAnnotations(as: List[Annotation]): ShapeExpr =
    this.copy(annotations = maybeAddList(annotations, as))
  override def addSemActs(as: List[SemAct]): ShapeExpr =
    this.copy(actions = maybeAddList(actions, as))

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

/** Declares an abstract shape expression
  */
case class ShapeDecl(
    lbl: ShapeLabel,
    shapeExpr: ShapeExpr,
    _abstract: Boolean = false
) extends ShapeExpr {

  override def id = lbl.some
  override def addId(lbl: ShapeLabel): ShapeDecl = this.copy(lbl = lbl)
  override def rmId = this

  def isVirtual: Boolean = _abstract

  override def paths(schema: AbstractSchema): Either[String, Set[Path]] =
    shapeExpr.paths(schema)

  override def addAnnotations(as: List[Annotation]): ShapeExpr =
    this.copy(shapeExpr = shapeExpr.addAnnotations(as))

  override def addSemActs(as: List[SemAct]): ShapeExpr =
    this.copy(shapeExpr = shapeExpr.addSemActs(as))

  override def relativize(base: IRI): ShapeDecl =
    this.copy(
      lbl = lbl.relativize(base),
      shapeExpr = shapeExpr.relativize(base)
    )

}

object ShapeDecl {}
