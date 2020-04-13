package es.weso.shex

import es.weso.rdf.PrefixMap
import es.weso.rdf.nodes.IRI
import cats._
import cats.data._
import cats.implicits._

trait ShapeExpr extends Product with Serializable {
  def id: Option[ShapeLabel]
  def addId(lbl: ShapeLabel): ShapeExpr

  def showPrefixMap(pm: PrefixMap) = {
    import es.weso.shex.compact.CompactShow._
    showShapeExpr(this, pm)
  }

  def paths(schema: AbstractSchema): Either[String, Set[Path]]
  def addAnnotations(as: List[Annotation]): ShapeExpr
  def addSemActs(as: List[SemAct]): ShapeExpr

  def relativize(base: IRI): ShapeExpr

  def hasNoReference(schema: AbstractSchema): Boolean = { 
    getShapeRefs(schema).fold(e => false, _.isEmpty)
  }

  def isSimple(schema: AbstractSchema): Boolean = this match {
    case s: Shape if (s == Shape.empty) => true
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

