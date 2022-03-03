package es.weso.shapepath.schemamappings

// import com.typesafe.scalalogging.LazyLogging
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
import es.weso.shapepath.parser.SchemaMappingsDocParser._
import es.weso.shapepath.schemamappings.SchemaMappingsParser._
import es.weso.shex.{BNodeLabel, IRILabel, ShapeLabel}

import scala.jdk.CollectionConverters._
import es.weso.rdf.Prefix

/**
 * Visits the AST and builds the corresponding abstract syntax
 */
class SchemaMappingsMaker extends SchemaMappingsDocBaseVisitor[Any] {

  type Directive = Either[(Prefix, IRI),  // Prefix decl
                   Either[IRI,            // Base decl
                          IRI             // Import decl
                     ]]


  override def visitSchemaMappingsDoc(ctx: SchemaMappingsDocContext): Builder[SchemaMappings] = 
  for {
    directives <- visitList(visitDirective, ctx.directive())
    mappings <- visitMappings(ctx.mappings())
    // visitShapePathExpr(ctx.shapePathExpr())
    state <- getState
  } yield SchemaMappings.empty.copy(prefixMap = state.prefixMap, mappings = mappings)

  override def visitMappings(ctx: MappingsContext): Builder[List[SchemaMapping]] = 
    for {
      mappings <- visitList(visitMapping, ctx.mapping())
    } yield mappings

  override def visitMapping(ctx: MappingContext): Builder[SchemaMapping] = 
    for {
      shapePath <- visitShapePathExpr(ctx.shapePathExpr())
      iri <- visitIri(ctx.iri())
    } yield SchemaMapping(shapePath,iri)

  override def visitDirective(
    ctx: DirectiveContext): Builder[Directive] = ctx match {
    case _ if (isDefined(ctx.baseDecl())) => for {
        iri <- visitBaseDecl(ctx.baseDecl())
      } yield Right(Left(iri))
    case _ if (isDefined(ctx.prefixDecl())) => for {
        p <- visitPrefixDecl(ctx.prefixDecl())
      } yield Left(p)
    case _ if (isDefined(ctx.importDecl())) => for {
      iri <- visitImportDecl(ctx.importDecl())
    } yield Right(Right(iri))
    case _ => err("visitDirective: unknown directive")
  }


  override def visitImportDecl(ctx: ImportDeclContext): Builder[IRI] = for {
    iri <- visitIri(ctx.iri())
  } yield iri

  override def visitBaseDecl(ctx: BaseDeclContext): Builder[IRI] = {
    for {
     previousBase <- getBase
     baseIri <- extractIRIfromIRIREF(ctx.IRIREF().getText, previousBase)
      _ <- addBase(baseIri)
    } yield baseIri
  }

  override def visitPrefixDecl(ctx: PrefixDeclContext): Builder[(Prefix, IRI)] = {
    if (ctx.PNAME_NS() == null) err("Invalid prefix declaration")
    else {
//    println(s"visitPrefixDecl: ${ctx.PNAME_NS()}")
//    println(s"visitPrefixDecl pnameNs.getText: ${ctx.PNAME_NS().getText}")
    val prefix = Prefix(ctx.PNAME_NS().getText.init)
    for {
      iri <- extractIRIfromIRIREF(ctx.IRIREF().getText, None)
      _   <- addPrefix(prefix, iri)
    } yield (prefix, iri)
    }
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
      case _ => err("visitShapePathExpr: unknown ctx")
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
    } yield ExprStep(maybeCtx,exprIndex,List())
   /* case cctx : ContextStepContext => for {
      context <- visitContextTest(cctx.contextTest())
    } yield ContextStep(context)
    case _ => err("visitStepExpr: unknown context: $ctx / ${ctx.getClass.getName}") */
  }/*for {
   maybeCtx <- visitContextTest(ctx.contextTest())
   exprIndex <- visitExprIndex(ctx.exprIndex())
  } yield ExprStep(maybeCtx,exprIndex) */

  override def visitContextTest(ctx: ContextTestContext): Builder[ContextType] =
      ctx match {
        case _ if (isDefined(ctx.shapeExprContext())) => for {
         shapeExprContext <- visitShapeExprContext(ctx.shapeExprContext())
        } yield shapeExprContext
        case _ if (isDefined(ctx.tripleExprContext())) => for {
          tripleExprContext <- visitTripleExprContext(ctx.tripleExprContext())
        } yield tripleExprContext
        case _ => err("visitContextTest: unknown ctx = $ctx")
      }

  override def visitShapeExprContext(ctx: ShapeExprContextContext): Builder[ContextType] = ctx match {
    case _ if isDefined(ctx.KW_ShapeAnd()) => ok(ShapeAndType)
    case _ if isDefined(ctx.KW_ShapeOr()) => ok(ShapeOrType)
    case _ if isDefined(ctx.KW_ShapeNot()) => ok(ShapeNotType)
    case _ if isDefined(ctx.KW_NodeConstraint()) => ok(NodeConstraintType)
    case _ if isDefined(ctx.KW_Shape()) => ok(ShapeType)
    case _ => err("visitShapeExprContext: unknown ctx: $ctx")
  }

  override def visitTripleExprContext(ctx: TripleExprContextContext): Builder[ContextType] = ctx match {
    case _ if isDefined(ctx.KW_EachOf()) => ok(EachOfType)
    case _ if isDefined(ctx.KW_OneOf()) => ok(OneOfType)
    case _ if isDefined(ctx.KW_TripleConstraint()) => ok(TripleConstraintType)
    case _ => err("visitTripleExprContext: unknown ctx: $ctx")
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
    case _ => err("visitExprIndex: unknown ctx: $ctx")
  }

  override def visitShapeExprIndex(ctx: ShapeExprIndexContext): Builder[ShapeExprIndex] = ctx match {
    case _ if isDefined(ctx.shapeExprLabel()) =>
      for {
        lbl <- visitShapeExprLabel(ctx.shapeExprLabel())
      } yield ShapeLabelIndex(lbl)
    case _ if isDefined(ctx.INTEGER()) =>
      for { n <- getInteger(ctx.INTEGER().getText())
      } yield IntShapeIndex(n)
    case _ => err("visitShapeExprIndex: unknown ctx $ctx")
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
    case _ => err("visitShapeExprIndex: unknown ctx $ctx")
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
    case _ => err("visitShapeExprLabel: unknown ctx $ctx")
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
      case s => err("IRIREF: $s does not match <...>")
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
     case _ => err[ShapeLabel]("visitTripleExprLabel: unknown ctx: $ctx")
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
          err("Prefix $prefix not found in current prefix map $prefixMap")
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
        err("Cannot get integer from $str")
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
