package es.weso.shapepath

import cats._
import cats.data._
import cats.implicits._
import es.weso.shapepath.compact.Parser
import Step._
import cats.implicits.catsSyntaxFlatMapOps
import es.weso.shex.implicits.showShEx._
import es.weso.rdf.PrefixMap
import es.weso.shex._
import es.weso.rdf.nodes._

case class ShapePath(
    startsWithRoot: Boolean,
    steps: List[Step]
) {
  def showQualify(pm: PrefixMap): String =
    (if (startsWithRoot) "/" else "") + steps.map(_.showQualify(pm)).mkString("/")

  def addSteps(steps: List[Step]): ShapePath =
    this.copy(steps = this.steps ++ steps)
}

object ShapePath {

  def empty: ShapePath = ShapePath(startsWithRoot = false, List())

  def fromTypePredicates(shapeType: ShapeNodeType, preds: List[Predicate]): ShapePath =
    throw new RuntimeException(s"fromTypePredicates: not implemented")

  def fromPredicates(preds: List[Predicate]): ShapePath =
    throw new RuntimeException(s"fromPredicates: not implemented")

  /** Evaluate a shapePath
    * @param p shape path
    * @param s schema
    * @param maybeValue initial value. If not provided it is initialized to the list of shapes in s
    * @return A pair containing a list of processing errors (empty if no error) and a result value
    */
  def eval(
      p: ShapePath,
      s: Schema,
      maybeValue: Option[Value] = None
  ): (List[ProcessingError], Value) =
    evaluateShapePath(p, s, maybeValue.getOrElse(Value(s.localShapes.map(ShapeExprItem(_))))).run

  def replace(
      p: ShapePath,
      s: Schema,
      maybeValue: Option[Value] = None,
      newShapeNode: ShapeNode
  ): Ior[List[ProcessingError], Schema] =
    replaceShapePath(p, s, newShapeNode)

  lazy val availableFormats: NonEmptyList[String] = NonEmptyList("Compact", List())
  lazy val defaultFormat: String = availableFormats.head

  /** Parse a shapePath from a string
    * @param str
    * @param format
    * @param base
    * @return either an error string or a ShapePath
    */
  def fromString(
      str: String,
      format: String = "Compact",
      base: Option[IRI] = None,
      prefixMap: PrefixMap = PrefixMap.empty
  ): Either[String, ShapePath] =
    format.toLowerCase match {
      case "compact" => Parser.parseShapePath(str, base, prefixMap)
      case _         => Left(s"Unsupported input format: $format")
    }

  private type Comp[A] = Writer[List[ProcessingError], A]

  private def ok[A](x: A): Comp[A] = x.pure[Comp]

  private def err(msg: String): Comp[Unit] = {
    val error: List[ProcessingError] = List(Err(msg))
    for {
      _ <- error.tell
    } yield ()
  }

  private def warning(msg: String): Comp[Unit] = {
    val warning: List[ProcessingError] = List(Warning(msg))
    for {
      _ <- warning.tell
    } yield ()
  }

  private def debug(msg: String): Comp[Unit] =
    // println(s"DEBUG: $msg")
    ().pure[Comp]

  /*  private def checkContext(ctx: Context)(item: Item): Boolean = ctx match {
    case ShapeAndCtx => item match {
      case ShapeExprItem(se) => se match {
        case _: ShapeAnd => true
        case _ => false
      }
      case _ => false
    }
    case ShapeOrCtx => item match {
      case ShapeExprItem(se) => se match {
        case _: ShapeOr => true
        case _ => false
      }
      case _ => false
    }
    case ShapeNotCtx => item match {
      case ShapeExprItem(se) => se match {
        case _: ShapeNot => true
        case _ => false
      }
      case _ => false
    }
    case NodeConstraintCtx => item match {
      case ShapeExprItem(se) => se match {
        case _: NodeConstraint => true
        case _ => false
      }
      case _ => false
    }
    case ShapeCtx => item match {
      case ShapeExprItem(se) => se match {
        case _: Shape => true
        case _ => false
      }
      case _ => false
    }
    case EachOfCtx => item match {
      case TripleExprItem(te) => te match {
        case _:EachOf => true
        case _ => false
      }
      case _ => false
    }
    case OneOfCtx => item match {
      case TripleExprItem(te) => te match {
        case _:OneOf => true
        case _ => false
      }
      case _ => false
    }
    case TripleConstraintCtx => item match {
      case TripleExprItem(te) => te match {
        case _:TripleConstraint => true
        case _ => false
      }
      case _ => false
    }
  } */
  private def checkContext(context: ContextType, item: ShapeNode): Comp[List[ShapeNode]] =
    throw new RuntimeException(s"checkContext: not implemented")

  private def matchShapeExprId(lbl: ShapeLabel)(se: ShapeExpr): Boolean = se.id match {
    case None        => false
    case Some(idLbl) => idLbl == lbl
  }

  private def noValue: Value = Value(List())

  private def hasPredicate(expr: TripleExpr, label: ShapeLabel): Comp[Boolean] = expr match {
    case tc: TripleConstraint =>
      label match {
        case i: IRILabel => ok(i.iri == tc.predicate)
        case _           => err(s"Label: ${label.show} must be an IRI") >> ok(false)
      }
    case _ => ok(false)
  }

  /** Get element from a list at position n
    * If the list doesn't have enough elements, returns the empty list
    * @param ls list
    * @param n position
    * @tparam A
    * @return List containing the element or empty
    *         Examples:
    *         getElementAt(List['a','b'.'c'], 2) = List('b')
    *         getElementAt(List['a','b'.'c'], 5) = List()
    */
  private def getElementAt[A](ls: List[A], n: Int): List[A] =
    ls.slice(n - 1, n)

  private def undef(msg: String, current: Value): Comp[Value] = err(msg) >> ok(current)

  private def evaluateIndex(items: List[ShapeNode], index: ExprIndex): Comp[Value] = {
    val zero: Value = noValue
    def cmb(current: Value, item: ShapeNode): Comp[Value] =
      item match {
        /*      case SchemaItem(s) => index match {
        case IntShapeIndex(idx) => Value(s.localShapes.slice(idx - 1,1).map(ShapeExprItem(_))).pure[Comp]
        case ShapeLabelIndex(lbl) => Value(s.localShapes.filter(matchShapeExprId(lbl)).map(ShapeExprItem(_))).pure[Comp]
        case _ => warning(s"Index ${index.show} applied to schema item ${s.show}")
      } */
        case ShapeExprItem(se) =>
          se match {
            case sa: ShapeAnd =>
              index match {
                case IntShapeIndex(n) =>
                  ok(current.add(getElementAt(sa.shapeExprs, n).map(ShapeExprItem(_))))
                case _ =>
                  // warning(s"ShapeAnd: evaluating index ${index.show} returns no item") >>
                  ok(current)
              }
            case so: ShapeOr =>
              index match {
                case IntShapeIndex(n) =>
                  ok(current.add(getElementAt(so.shapeExprs, n).map(ShapeExprItem(_))))
                case _ =>
                  // warning(s"ShapeOr: evaluating index ${index.show} returns no item") >>
                  ok(current)
              }
            case _: ShapeNot =>
              warning(s"ShapeNot: evaluating index ${index.show} returns no item") >>
                ok(current)
            case sd: ShapeDecl =>
              cmb(current, ShapeExprItem(sd.shapeExpr))
            case s: Shape =>
              index match {
                case ShapeLabelIndex(lbl) =>
                  s.id match {
                    case None =>
                      warning(s"Index: ${index.show} accessing shape without label") >>
                        ok(current)
                    case Some(slbl) =>
                      if (slbl == lbl)
                        ok(current.add(s))
                      else
                        warning(s"Index: ${index.show} accessing shape with label ${slbl.show}") >>
                          ok(current)
                  }
                case _: LabelTripleExprIndex | _: IntTripleExprIndex =>
                  s.expression match {
                    case None =>
                      warning(
                        s"Index: ${index.show} accessing shape without expression: ${s.show}"
                      ) >>
                        ok(current)
                    case Some(te) =>
                      evaluateIndex(List(TripleExprItem(te)), index).flatMap(newValue =>
                        ok(current.add(newValue))
                      )
                  }
                case _ =>
                  err(s"evaluateIndex: Match index shape: Unimplemented index ${index}") >>
                    ok(current)
              }
            case _ =>
              err(s"evaluateIndex: Unimplemented ShapeExprItem(${se.show})") >>
                ok(current)
          }
        case TripleExprItem(te) =>
          te match {
            case eo: EachOf =>
              index match {
                case LabelTripleExprIndex(lbl, maybeN) =>
                  for {
                    tcs <- TraverseFilter[List].filterA(eo.expressions)(hasPredicate(_, lbl))
                    tcsFiltered = maybeN match {
                      case None    => tcs
                      case Some(n) => getElementAt(tcs, n)
                    }
                  } yield current.add(Value(tcsFiltered.map(TripleExprItem(_))))
                case IntTripleExprIndex(n) =>
                  ok(current.add(getElementAt(eo.expressions, n).map(TripleExprItem(_))))
                case _ =>
                  err(s"Matching TripleExprItem EachOf: unknown index ${index.show}") >>
                    ok(current)
              }
            case tc: TripleConstraint =>
              index match {
                case LabelTripleExprIndex(lbl, maybeN) =>
                  for {
                    check <- hasPredicate(tc, lbl)
                    r <-
                      if (check) maybeN match {
                        case None | Some(1) => ok(current.add(tc))
                        case Some(other) =>
                          err(
                            s"Evaluate index tripleConstraint ${tc} with index ${index.show}. Value should be 1 and is ${other}"
                          ) >>
                            ok(current)
                      }
                      else
                        warning(
                          s"evaluateIndex tripleConstraint ${tc} doesn't match ${index.show}. Predicate ${tc.predicate.show} != ${lbl.show}"
                        ) >>
                          ok(current)
                  } yield r
                case IntTripleExprIndex(1) =>
                  tc.valueExpr match {
                    case None =>
                      warning(
                        s"Matching triple constraint ${tc.show} with index 1 but no ShapeExpr: nothing to add"
                      ) >>
                        ok(current)
                    case Some(se) =>
                      ok(current.add(se))
                  }
                case IntTripleExprIndex(other) =>
                  err(
                    s"Matching triple constraint ${tc.show} with int index: ${index.show} (it should be 1"
                  ) >>
                    ok(current)
                case _ =>
                  err(s"Matching triple constraint ${tc.show} with unknown index: ${index.show}") >>
                    ok(current)
              }
            case _ =>
              err(s"Matching TripleExprItem: unknown: ${te.show}") >>
                ok(current)
          }
        case _ =>
          err(s"Unknown item: ${item.show}") >>
            ok(current)
      }
    Foldable[List].foldM[Comp, ShapeNode, Value](items, zero)(cmb)
  }

  private def cmb(
      ctx: ContextType
  )(current: List[ShapeNode], item: ShapeNode): Comp[List[ShapeNode]] = for {
    next <- checkContext(ctx, item)
  } yield current ++ next

  private def evaluateStep(s: Schema)(current: Comp[Value], step: Step): Comp[Value] = step match {
    case nt: NodeTestStep =>
      nt.axis match {
        case Child =>
          for {
            currentValue <- current
          } yield currentValue.evalChild(nt.nodeTest)
        case NestedShapeExpr =>
          for {
            currentValue <- current
          } yield currentValue.evalNestedShapeExpr(nt.nodeTest)
        case _ =>
          err(s"Not implemented axis: ${nt.axis}") *>
            current
      }
    case es: ExprStep =>
      // println(s"ExprStep: ${step.show}")
      es.maybeType match {
        case None =>
          for {
            currentValue <- current
            _ <- debug(s" Current value: ${currentValue.show}")
            value <- evaluateIndex(currentValue.items, es.exprIndex)
            _ <- debug(s" New value after evaluateIndex: ${value.show}")
          } yield value
        case Some(ctx) =>
          for {
            currentValue <- current
            _ <- debug(s"Context step ${ctx.show}\nvalue: ${currentValue.show}")
            // (matched,unmatched) = currentValue.items.partition(checkContext(ctx))
            matched <- currentValue.items.foldM[Comp, List[ShapeNode]](List())(cmb(ctx))
            _ <- debug(s"Matched ${matched}")
            // _ <- debug(s"Unmatched ${unmatched}")
            newValue <- evaluateIndex(matched, es.exprIndex)
            _ <- debug(s"newValue ${newValue.show}")
          } yield newValue
      }
    /*    case ContextStep(ctx) => for {
     currentValue <- current
     matched <- currentValue.items.foldM[Comp,List[ShapeNode]](List())(cmb(ctx))
    } yield Value(matched)  */
  }

  private def evaluateShapePath(p: ShapePath, s: Schema, v: Value): Comp[Value] = {
    val zero: Comp[Value] = if (p.startsWithRoot) {
      val v = Value(s.localShapes.map(ShapeExprItem(_)))
      // println(s"Starts with root\nValue = ${v.show}")
      v.pure[Comp]
    } else v.pure[Comp]

    p.steps.foldLeft(zero)(evaluateStep(s))
  }

  type CompReplace[A] = Ior[List[ProcessingError], A]

  def rerr[A](msg: String): CompReplace[A] =
    Ior.Left(List(Err(msg)))
  def rwarn[A](msg: String, x: A): CompReplace[A] =
    Ior.Both(List(Warning(msg)), x)

  def info(msg: String): CompReplace[Unit] = {
    println(msg)
    Ior.Right(())
  }
  def okr[A](x: A): CompReplace[A] = Ior.Right(x)

  private def replaceTripleExprLabel(
      te: TripleExpr,
      sourceIri: IRI,
      newItem: ShapeNode
  ): CompReplace[TripleExpr] = te match {
    case eo: EachOf =>
      for {
        eos <- eo.expressions.map(replaceTripleExprLabel(_, sourceIri, newItem)).sequence
      } yield eo.copy(expressions = eos)
    case oo: OneOf =>
      for {
        os <- oo.expressions.map(replaceTripleExprLabel(_, sourceIri, newItem)).sequence
      } yield oo.copy(expressions = os)
    case tc: TripleConstraint =>
      newItem match {
        case IRIItem(iri) =>
          // println(s"replaceTripleExprLabel=${iri}, tc.predicate = ${tc.predicate}, newItem=${iri}")
          if (tc.predicate == sourceIri) okr(tc.copy(predicate = iri))
          else okr(tc)
        case _ => rerr(s"replaceTripleExprLabel: Not implemented at tripleConstraint: ${newItem}")
      }
    case _ => okr(te)
  }

  private def replaceShapeExprSteps(
      se: ShapeExpr,
      steps: List[Step],
      schema: Schema,
      newItem: ShapeNode
  ): CompReplace[ShapeExpr] = steps match {
    case Nil => okr(se)
    case ExprStep(None, LabelTripleExprIndex(IRILabel(iri), None), _) :: Nil =>
      se match {
        case sd: ShapeDecl => throw new RuntimeException(s"TODO!! ???")
        case s: Shape =>
          s.expression match {
            case None => okr(se)
            case Some(te) =>
              for {
                newTe <- replaceTripleExprLabel(te, iri, newItem)
              } yield s.copy(expression = Some(newTe))
          }
        case _ => rerr(s"replaceShapeExprSteps: step: ${iri.show} se=${se}")
      }
    case _ => rerr(s"replaceShapeExprSteps: several steps ${steps}...")
  }

  private def updateLabel(schema: Schema, newShapeExpr: ShapeExpr): CompReplace[Schema] =
    okr(schema.addShape(newShapeExpr))

  private def replaceShapePath(
      p: ShapePath,
      s: Schema,
      newItem: ShapeNode
  ): CompReplace[Schema] =
    p.steps match {
      case Nil => okr(s)
      case ExprStep(None, ShapeLabelIndex(lbl), _) :: rest =>
        s.getShape(lbl) match {
          case Left(err) => rwarn(s"Not found label: ${err}", s)
          case Right(se) =>
            for {
              newShapeExpr <- replaceShapeExprSteps(se, rest, s, newItem)
              _ <- info(s"newShapeExpr: ${newShapeExpr}")
              newSchema <- updateLabel(s, newShapeExpr)
            } yield
            // println(s"ExprStep: ${lbl}...newSchema: ${newSchema}")
            newSchema
        }
      case step :: rest => rerr(s"Not implemented step ${step} yet")
    }

}
