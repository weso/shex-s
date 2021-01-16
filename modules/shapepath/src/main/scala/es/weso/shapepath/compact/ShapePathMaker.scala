package es.weso.shapepath.compact
import com.typesafe.scalalogging.LazyLogging
import cats.implicits._
// import es.weso.rdf.Prefix
import es.weso.rdf.nodes._
// import es.weso.rdf.PREFIXES._
import es.weso.shapepath._
// import es.weso.shex.parser._
// import es.weso.shex.values._
import es.weso.utils.StrUtils._
// import es.weso.rdf.operations.Comparisons._
import es.weso.shapepath.parser._
import es.weso.shapepath.parser.ShapePathDocParser._
import es.weso.shapepath.compact.Parser._
import es.weso.shex.{BNodeLabel, IRILabel, ShapeLabel}

import scala.jdk.CollectionConverters._

/**
 * Visits the AST and builds the corresponding abstract syntax
 */
class ShapePathMaker extends ShapePathDocBaseVisitor[Any] with LazyLogging {

  override def visitShapePathDoc(ctx: ShapePathDocContext): Builder[ShapePath] = {
    visitShapePathExpr(ctx.shapePathExpr())
  }

  override def visitShapePathExpr(ctx: ShapePathExprContext): Builder[ShapePath] = {
    ctx match {
      case _ if isDefined(ctx.absolutePathExpr()) => for {
        steps <- visitAbsolutePathExpr(ctx.absolutePathExpr())
      } yield ShapePath(true, steps)
      case _ if isDefined(ctx.relativePathExpr()) =>
        for {
          steps <- visitRelativePathExpr(ctx.relativePathExpr())
        } yield ShapePath(false,steps)
      case _ => err(s"visitShapePathExpr: unknown ctx: $ctx")
    }
  }

  override def visitAbsolutePathExpr(ctx: AbsolutePathExprContext): Builder[List[Step]] =
    visitRelativePathExpr(ctx.relativePathExpr())

  override def visitRelativePathExpr(ctx: RelativePathExprContext): Builder[List[Step]] = {
    for {
      steps <- visitList(visitStepExpr, ctx.stepExpr())
    } yield steps
  }

  def visitStepExpr(ctx: StepExprContext): Builder[Step] = ctx match {
    case ectx : ExprIndexStepContext => for {
      maybeCtx <- visitOpt(visitContextTest, ectx.contextTest())
      exprIndex <- visitExprIndex(ectx.exprIndex())
    } yield ExprStep(maybeCtx,exprIndex)
    case cctx : ContextStepContext => for {
      context <- visitContextTest(cctx.contextTest())
    } yield ContextStep(context)
    case _ => err(s"visitStepExpr: unknown context: $ctx / ${ctx.getClass.getName}")
  }/*for {
   maybeCtx <- visitContextTest(ctx.contextTest())
   exprIndex <- visitExprIndex(ctx.exprIndex())
  } yield ExprStep(maybeCtx,exprIndex) */

  override def visitContextTest(ctx: ContextTestContext): Builder[Context] =
      ctx match {
        case _ if (isDefined(ctx.shapeExprContext())) => for {
         shapeExprContext <- visitShapeExprContext(ctx.shapeExprContext())
        } yield shapeExprContext
        case _ if (isDefined(ctx.tripleExprContext())) => for {
          tripleExprContext <- visitTripleExprContext(ctx.tripleExprContext())
        } yield tripleExprContext
        case _ => err(s"visitContextTest: unknown ctx = $ctx")
      }

  override def visitShapeExprContext(ctx: ShapeExprContextContext): Builder[Context] = ctx match {
    case _ if isDefined(ctx.KW_ShapeAnd()) => ok(ShapeAndCtx)
    case _ if isDefined(ctx.KW_ShapeOr()) => ok(ShapeOrCtx)
    case _ if isDefined(ctx.KW_ShapeNot()) => ok(ShapeNotCtx)
    case _ if isDefined(ctx.KW_NodeConstraint()) => ok(NodeConstraintCtx)
    case _ if isDefined(ctx.KW_Shape()) => ok(ShapeCtx)
    case _ => err(s"visitShapeExprContext: unknown ctx: $ctx")
  }

  override def visitTripleExprContext(ctx: TripleExprContextContext): Builder[Context] = ctx match {
    case _ if isDefined(ctx.KW_EachOf()) => ok(EachOfCtx)
    case _ if isDefined(ctx.KW_OneOf()) => ok(OneOfCtx)
    case _ if isDefined(ctx.KW_TripleConstraint()) => ok(TripleConstraintCtx)
    case _ => err(s"visitTripleExprContext: unknown ctx: $ctx")
  }

  override def visitExprIndex(ctx: ExprIndexContext): Builder[ExprIndex] = ctx match {
    case _ if isDefined(ctx.shapeExprIndex()) => for {
      idx <- visitShapeExprIndex(ctx.shapeExprIndex())
    } yield {
      val eidx: ExprIndex = idx
      eidx
    }
    case _ if isDefined(ctx.tripleExprIndex()) => for {
      idx <- visitTripleExprIndex(ctx.tripleExprIndex())
    } yield {
      val eidx : ExprIndex = idx
      eidx
    }
    case _ => err(s"visitExprIndex: unknown ctx: $ctx")
  }

  override def visitShapeExprIndex(ctx: ShapeExprIndexContext): Builder[ShapeExprIndex] = ctx match {
    case _ if isDefined(ctx.shapeExprLabel()) =>
      for {
        lbl <- visitShapeExprLabel(ctx.shapeExprLabel())
      } yield ShapeLabelIndex(lbl)
    case _ if isDefined(ctx.INTEGER()) =>
      for { n <- getInteger(ctx.INTEGER().getText())
      } yield IntShapeIndex(n)
    case _ => err(s"visitShapeExprIndex: unknown ctx $ctx")
  }

  override def visitTripleExprIndex(ctx: TripleExprIndexContext): Builder[TripleExprIndex] = ctx match {
    case _ if isDefined(ctx.tripleExprLabel()) =>
      for {
        pair <- visitTripleExprLabel(ctx.tripleExprLabel())
      } yield {
        val (lbl, maybeInt) = pair
        LabelTripleExprIndex(lbl, maybeInt)
      }
    case _ if isDefined(ctx.INTEGER()) =>
      for {
        n <- getInteger(ctx.INTEGER().getText())
      } yield IntTripleExprIndex(n)
    case _ => err(s"visitShapeExprIndex: unknown ctx $ctx")
  }

  override def visitShapeExprLabel(ctx: ShapeExprLabelContext): Builder[ShapeLabel] = ctx match {
    case _ if isDefined(ctx.iri()) => for {
      iri <- visitIri(ctx.iri())
    } yield {
      IRILabel(iri)
    }
    case _ if isDefined(ctx.blankNodeLabel()) => for {
      lbl <- visitBlankNodeLabel(ctx.blankNodeLabel())
    } yield
     lbl
    case _ => err(s"visitShapeExprLabel: unknown ctx $ctx")
  }

  override def visitBlankNodeLabel(ctx: BlankNodeLabelContext): Builder[BNodeLabel] = for {
    bnode <- visitBlankNode(ctx.blankNode())
  } yield BNodeLabel(bnode)

  override def visitBlankNode(ctx: BlankNodeContext): Builder[BNode] = {
    ok(BNode(removeUnderscore(ctx.BLANK_NODE_LABEL().getText())))
  }

  def removeUnderscore(str: String): String =
    str.drop(2)

  def extractIRIfromIRIREF(d: String, base: Option[IRI]): Builder[IRI] = {
    val str = unescapeIRI(d)
    val iriRef = "^<(.*)>$".r
    str match {
      case iriRef(i) => IRI.fromString(i,base).fold(
        str => err(str),
        i => {
          base match {
            case None => ok(i)
            case Some(b) => {
              if (b.uri.toASCIIString.startsWith("file:///")) {
                // For some reason, when resolving a file:///foo iri, the system returns file:/foo
                // The following code keeps the file:/// part
                ok(IRI(b.uri.resolve(i.uri).toASCIIString.replaceFirst("file:/","file:///")))
              } else {
                ok(IRI(b.uri.resolve(i.uri)))
              }
            }
          }
        })
      case s => err(s"IRIREF: $s does not match <...>")
    }
  }

  override def visitTripleExprLabel(ctx: TripleExprLabelContext): Builder[(ShapeLabel, Option[Int])] = for {
   maybeInt <- if (isDefined(ctx.INTEGER())) {
     for {
       n <- getInteger(ctx.INTEGER().getText())
     } yield Some(n)
   } else ok(none[Int])
   lbl <- ctx match {
     case _ if isDefined(ctx.iri()) => for {
       iri <- visitIri(ctx.iri())
     } yield {
       IRILabel(iri)
     }

     case _ if isDefined(ctx.blankNodeLabel()) => for {
       lbl <- visitBlankNodeLabel(ctx.blankNodeLabel())
     } yield
       lbl
     case _ => err(s"visitTripleExprLabel: unknown ctx: $ctx")
   }
  } yield (lbl, maybeInt)

  override def visitIri(ctx: IriContext): Builder[IRI] =
    if (isDefined(ctx.IRIREF())) for {
      base <- getBase
      iri <- extractIRIfromIRIREF(ctx.IRIREF().getText, base)
    } yield iri
    else for {
      prefixedName <- visitPrefixedName(ctx.prefixedName())
      iri <- resolve(prefixedName)
    } yield iri

  def resolve(prefixedName: String): Builder[IRI] = {
    val (prefix, local) = splitPrefix(prefixedName)
    // logger.info(s"Resolve. prefix: $prefix local: $local Prefixed name: $prefixedName")
    getPrefixMap.flatMap(prefixMap =>
      prefixMap.getIRI(prefix) match {
        case None =>
          err(s"Prefix $prefix not found in current prefix map $prefixMap")
        case Some(iri) =>
          ok(iri + local)
      })
  }

  def splitPrefix(str: String): (String, String) = {
    if (str contains ':') {
      val (prefix, name) = str.splitAt(str.lastIndexOf(':'))
      (prefix, name.tail)
    } else {
      ("", str)
    }
  }

  override def visitPrefixedName(ctx: PrefixedNameContext): Builder[String] = {
    ok(ctx.getText())
  }


  // Some generic utils

  def getInteger(str: String): Builder[Int] = {
    try {
      ok(str.toInt)
    } catch {
      case _: NumberFormatException =>
        err(s"Cannot get integer from $str")
    }
  }

  def isDefined[A](x: A): Boolean = x != null

  def visitList[A, B](visitFn: A => Builder[B],
                      ls: java.util.List[A]
                     ): Builder[List[B]] = {
    val bs: List[Builder[B]] = ls.asScala.toList.map(visitFn(_))
    sequence(bs)
  }

  def visitOpt[A, B](
    visitFn: A => Builder[B],
    v: A): Builder[Option[B]] =
    if (isDefined(v)) visitFn(v).map(Some(_))
    else ok(None)

}
