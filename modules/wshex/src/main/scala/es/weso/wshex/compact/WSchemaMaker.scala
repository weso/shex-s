package es.weso.wshex.compact

import es.weso.rdf.Prefix
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.wshex.parser.WShExDocBaseVisitor
import es.weso.wshex.compact.Parser._
import es.weso.wshex.parser.WShExDocParser.{StringContext => ShExStringContext, _}
import es.weso.shex.values._
import es.weso.utils.StrUtils._
import es.weso.rdf.operations.Comparisons._
import scala.jdk.CollectionConverters._
import es.weso.rdf.locations.Location
import org.antlr.v4.runtime.Token
import es.weso.shex.{
  FractionDigits,
  IntMax,
  Length,
  Max,
  MaxExclusive,
  MaxInclusive,
  MaxLength,
  MinExclusive,
  MinInclusive,
  MinLength,
  NumericFacet,
  Pattern,
  Star,
  StringFacet,
  TotalDigits,
  XsFacet
}
import es.weso.wshex._
import es.weso.wbmodel._
import es.weso.rbe.interval._
import TripleConstraint._
import TermConstraint._
import cats.implicits._
import WNodeConstraint._
import ReferencesSpec._
import PropertySpec._
import PropertyConstraint._

/** Visits the AST and builds the corresponding ShEx abstract syntax
  */
class WSchemaMaker extends WShExDocBaseVisitor[Any] {

  type Start = Option[WShapeExpr]
  type NotStartAction = Either[Start, (ShapeLabel, WShapeExpr)]
  case class Cardinality(min: Int, max: IntOrUnbounded)
  type Directive = Either[
    (Prefix, IRI), // Prefix decl
    Either[
      IRI, // Base decl
      IRI // Import decl
    ]
  ]

  val star = Cardinality(0, Unbounded)
  val plus = Cardinality(1, Unbounded)
  val optional = Cardinality(0, IntLimit(1))
  val defaultCardinality = Cardinality(1, IntLimit(1))

  override def visitWShExDoc(
      ctx: WShExDocContext
  ): Builder[WSchema] =
    for {
      directives <- visitList(visitDirective, ctx.directive())
//      startActions <- visitStartActions(ctx.startActions())
//      notStartAction <- visitNotStartAction(ctx.notStartAction())
      statements <- visitList(visitStatement, ctx.statement())
      prefixMap <- getPrefixMap
      base <- getBase
      start <- getStart
      shapeMap <- getShapesMap
//      tripleExprMap <- getTripleExprMap
      labelLocationMap <- getLabelLocationMap
    } yield {
      val importIRIs = directives.collect { case Right(Right(iri)) =>
        iri
      }
      val pm = if (!prefixMap.isEmpty) Some(prefixMap) else None
      // val shapes = if (!shapeMap.isEmpty) Some(shapesMap2List(shapeMap)) else None
      // val optTripleExprMap = if (!tripleExprMap.isEmpty) Some(tripleExprMap) else None
      WSchema.empty
        .withPrefixMap(pm)
        .withBase(base)
        // .withStartActions(startActions)
        .withStart(start)
        .withShapesMap(shapeMap)
      // .withOptTripleExprMap(optTripleExprMap)
      // .withImports(importIRIs)
      // .withLabelLocationMap(Some(labelLocationMap))
    }

  /*  def shapesMap2List(sm: ShapesMap): List[WShapeExpr] =
    sm.map { case (lbl, se) => se.addId(lbl) }.toList */

  override def visitStatement(ctx: StatementContext): Builder[Unit] = ctx match {
    case _ if isDefined(ctx.start()) =>
      visitStart(ctx.start()).map(_ => ())
    case _ if isDefined(ctx.shapeExprDecl()) =>
      visitShapeExprDecl(ctx.shapeExprDecl()).map(_ => ())
  }
  /*
  override def visitNotStartAction(ctx: NotStartActionContext): Builder[NotStartAction] =
    if (ctx == null) ok(Left(None))
    else
      ctx match {
        case _ if isDefined(ctx.start()) =>
          for {
            s <- visitStart(ctx.start())
          } yield Left(s)
        case _ if isDefined(ctx.shapeExprDecl()) =>
          for {
            s <- visitShapeExprDecl(ctx.shapeExprDecl())
          } yield Right(s)
      }

  override def visitStartActions(ctx: StartActionsContext): Builder[Option[List[SemAct]]] =
    if (isDefined(ctx)) {
      val r: List[Builder[SemAct]] =
        ctx.semanticAction().asScala.map(visitSemanticAction(_)).toList
      sequence(r).map(Some(_))
    } else ok(None)
   */
  override def visitStart(ctx: StartContext): Builder[Option[(WShapeExpr)]] =
    // logger.info(s"Visiting start...$ctx")
    if (isDefined(ctx)) {
      // logger.info(s"is defined...")
      for {
        shapeExpr <- visitShapeExpression(ctx.shapeExpression())
        _ <-
          // logger.info(s"Shape expression for start: $shapeExpr")
          updateStart(Some(shapeExpr))
        // semActs <- visitSemanticActions(ctx.semanticActions())
      } yield shapeExpr.some
    } else ok(none)

  /*  override def visitSemanticActions(ctx: SemanticActionsContext): Builder[List[SemAct]] =
    for {
      ls <- visitList(visitSemanticAction, ctx.semanticAction())
    } yield {
      ls
    } */

  /*  override def visitSemanticAction(ctx: SemanticActionContext): Builder[SemAct] =
    for {
      iri <- visitIri(ctx.iri())
      code <- optBuilder(ctx.CODE()).map(opt => opt.map(_.getText()))
      str <- optMapBuilder(code, cleanCode)
    } yield SemAct(iri, str)

  def cleanCode(str: String): Builder[String] = {
    val codeRegex = "^\\{(.*)%\\}$".r
    str match {
      case codeRegex(c) => ok(unescapeCode(c))
      case _            => err(s"cleanCode: $str doesn't match regex $codeRegex")
    }
  }
   */
  def optBuilder[A](v: A): Builder[Option[A]] =
    if (v == null)
      ok(None)
    else
      ok(Some(v))

  def optMapBuilder[A, B](x: Option[A], f: A => Builder[B]): Builder[Option[B]] =
    x match {
      case None    => ok(None)
      case Some(v) => f(v).map(Some(_))
    }

  // TODO: Check more information about source
  private def getLocation(token: Token): Builder[Location] =
    ok(
      Location(
        line = token.getLine(),
        col = token.getCharPositionInLine(),
        tokenType = "label"
      )
    )

  override def visitShapeExprDecl(ctx: ShapeExprDeclContext): Builder[(ShapeLabel, WShapeExpr)] =
    for {
      label <- visitShapeExprLabel(ctx.shapeExprLabel())
      // _ <- info(s"Visited label $label")
      location <- getLocation(ctx.start)
      _ <- addLabelLocation(label, location)
      shapeExpr <- obtainShapeExpr(ctx)
      // _ <- info(s"Visited shapeExpr: $shapeExpr")
      /*W se <-
        /*if (isDefined(ctx.KW_ABSTRACT()))
          ok(ShapeDecl(label, shapeExpr))
        else */ ok(shapeExpr) */
      se = shapeExpr.withLabel(label)
      _ <- addShape(label, se)
    } yield (label, se)

  private def obtainShapeExpr(ctx: ShapeExprDeclContext): Builder[WShapeExpr] =
    /* if (isDefined(ctx.KW_EXTERNAL())) {
      // TODO: What happens if there are semantic actions after External??
      ok(ShapeExternal.empty)
    } else */
    visitShapeExpression(ctx.shapeExpression())

  override def visitShapeExpression(ctx: ShapeExpressionContext): Builder[WShapeExpr] =
    visitShapeOr(ctx.shapeOr())

  override def visitInlineShapeExpression(ctx: InlineShapeExpressionContext): Builder[WShapeExpr] =
    visitInlineShapeOr(ctx.inlineShapeOr())

  override def visitInlineShapeOr(ctx: InlineShapeOrContext): Builder[WShapeExpr] = for {
    shapes <- {
      val r: List[Builder[WShapeExpr]] =
        ctx.inlineShapeAnd().asScala.map(visitInlineShapeAnd(_)).toList
      sequence(r)
    }
  } yield mkShapeOr(shapes, Flatten)

  override def visitInlineShapeAnd(ctx: InlineShapeAndContext): Builder[WShapeExpr] =
    for {
      shapes <- {
        val r: List[Builder[WShapeExpr]] =
          ctx.inlineShapeNot().asScala.map(visitInlineShapeNot(_)).toList
        sequence(r)
      }
    } yield mkShapeAnd(shapes, Flatten)

  override def visitInlineShapeNot(ctx: InlineShapeNotContext): Builder[WShapeExpr] = for {
    shapeAtom <- visitInlineShapeAtom(ctx.inlineShapeAtom())
  } yield
    if (isDefined(ctx.negation()))
      WShapeNot(None, shapeAtom) // WShEx , None, None)
    else
      shapeAtom

  override def visitShapeOr(ctx: ShapeOrContext): Builder[WShapeExpr] = for {
    shapes <- {
      val r: List[Builder[WShapeExpr]] =
        ctx.shapeAnd().asScala.map(visitShapeAnd(_)).toList
      sequence(r)
    }
  } yield mkShapeOr(shapes, NO_Flatten)

  private def mkShapeAnd(shapes: List[WShapeExpr], flatten: Boolean): WShapeExpr = {
    val zero = List[WShapeExpr]()
    def next(c: WShapeExpr, rs: List[WShapeExpr]): List[WShapeExpr] = c match {
      case sa: WShapeAnd => sa.exprs ++ rs
      case _             => c :: rs
    }
    lazy val flattenShapes = shapes.foldRight(zero)(next)
    mkShapeOp(if (flatten) flattenShapes else shapes, WShapeAnd.fromShapeExprs)
  }

  private def mkShapeOr(shapes: List[WShapeExpr], flatten: Boolean): WShapeExpr = {
    val zero = List[WShapeExpr]()
    def next(c: WShapeExpr, rs: List[WShapeExpr]): List[WShapeExpr] = c match {
      case so: WShapeOr => so.exprs ++ rs
      case _            => c :: rs
    }
    lazy val flattenShapes = shapes.foldRight(zero)(next)
    mkShapeOp(if (flatten) flattenShapes else shapes, WShapeOr.fromShapeExprs)
  }

  private def mkShapeOp(shapes: List[WShapeExpr], op: List[WShapeExpr] => WShapeExpr): WShapeExpr =
    if (shapes.length == 1) shapes.head
    else op(shapes)

  override def visitShapeAnd(ctx: ShapeAndContext): Builder[WShapeExpr] =
    for {
      shapes <- {
        val r: List[Builder[WShapeExpr]] =
          ctx.shapeNot().asScala.map(visitShapeNot(_)).toList
        sequence(r)
      }
    } yield mkShapeAnd(shapes, NO_Flatten)

  private val NO_Flatten = false
  private val Flatten = true

  override def visitShapeNot(ctx: ShapeNotContext): Builder[WShapeExpr] = for {
    shapeAtom <- visitShapeAtom(ctx.shapeAtom())
  } yield
    if (isDefined(ctx.negation()))
      WShapeNot(None, shapeAtom) // , None, None)
    else shapeAtom

  private def visitShapeAtom(ctx: ShapeAtomContext): Builder[WShapeExpr] =
    ctx match {
      case s: ShapeAtomNonLitNodeConstraintContext =>
        for {
          nk <- visitNonLitNodeConstraint(s.nonLitNodeConstraint())
          sr <- visitOpt(visitShapeOrRef, s.shapeOrRef())
        } yield sr match {
          case None     => nk
          case Some(sa) => WShapeAnd(None, List(nk, sa)) // , None, None)
        }
      case s: ShapeAtomLitNodeConstraintContext =>
        for {
          nc <- visitLitNodeConstraint(s.litNodeConstraint())
        } yield nc
      case s: ShapeAtomShapeOrRefContext =>
        for {
          sr <- visitShapeOrRef(s.shapeOrRef())
          maybeNc <- visitOpt(visitNonLitNodeConstraint, s.nonLitNodeConstraint())
        } yield maybeNc match {
          case None     => sr
          case Some(nc) => WShapeAnd(None, List(sr, nc)) // W , None, None)
        }
      case s: ShapeAtomShapeExpressionContext =>
        visitShapeExpression(s.shapeExpression())
      case _: ShapeAtomAnyContext =>
        ok(emptyExpr)
      case _ => err(s"Internal error visitShapeAtom: unknown ctx $ctx")
    }

  def visitInlineLitNodeConstraint(
      ctx: InlineLitNodeConstraintContext
  ): Builder[WNodeConstraint] =
    ctx match {
      case s: NodeConstraintLiteralContext =>
        for {
          literalKind <- visitLiteralKind(s.literalKind())
          xsFacets <- visitList(visitXsFacet, s.xsFacet())
          _ <- checkFacets(xsFacets)
        } yield WNodeConstraint.nodeKind(WNodeKind.LiteralKind, xsFacets)
      /*W      case s: NodeConstraintNonLiteralContext =>
        for {
          nodeKind <- visitNonLiteralKind(s.nonLiteralKind())
          stringFacets <- visitList(visitStringFacet, s.stringFacet())
          _ <- checkFacets(stringFacets)
        } yield NodeConstraint.nodeKind(nodeKind, stringFacets) */
      case s: NodeConstraintDatatypeContext =>
        for {
          datatype <- visitDatatype(s.datatype())
          xsFacets <- visitList(visitXsFacet, s.xsFacet())
          _ <- checkFacets(xsFacets)
        } yield WNodeConstraint.datatype(datatype, xsFacets)
      case c: NodeConstraintValueSetContext =>
        for {
          vs <- visitValueSet(c.valueSet())
          xsFacets <- visitList(visitXsFacet, c.xsFacet())
          _ <- checkFacets(xsFacets)
        } yield WNodeConstraint.valueSet(vs, xsFacets)

      case c: NodeConstraintNumericFacetContext =>
        for {
          facets <- visitList(visitNumericFacet, c.numericFacet)
          _ <- checkFacets(facets)
        } yield WNodeConstraint.xsFacets(facets)
    }

  override def visitLiteralKind(ctx: LiteralKindContext): Builder[WNodeKind] =
    ctx match {
      case _ if isDefined(ctx.KW_LITERAL())          => ok(WNodeKind.LiteralKind)
      case _ if isDefined(ctx.KW_TIME())             => ok(WNodeKind.TimeKind)
      case _ if isDefined(ctx.KW_QUANTITY())         => ok(WNodeKind.QuantityKind)
      case _ if isDefined(ctx.KW_STRING())           => ok(WNodeKind.StringKind)
      case _ if isDefined(ctx.KW_MONOLINGUALTEXT())  => ok(WNodeKind.MonolingualTextKind)
      case _ if isDefined(ctx.KW_MULTILINGUALTEXT()) => ok(WNodeKind.MultilingualTextKind)
      case _ if isDefined(ctx.KW_GEOCOORDINATES())   => ok(WNodeKind.GeoCoodrinatesKind)
      case _ if isDefined(ctx.KW_GEOSHAPE())         => ok(WNodeKind.GeoShapeKind)
      case _ if isDefined(ctx.KW_MEDIA())            => ok(WNodeKind.MediaKind)
    }

  override def visitXsFacet(ctx: XsFacetContext): Builder[XsFacet] = ???

  override def visitLitNodeConstraint(ctx: LitNodeConstraintContext): Builder[WNodeConstraint] =
    for {
      nc <- visitInlineLitNodeConstraint(ctx.inlineLitNodeConstraint())
      // W semActs <- visitList(visitSemanticAction, ctx.semanticAction())
      // W anns <- visitList(visitAnnotation, ctx.annotation())
    } yield nc // W .addSemActs(semActs).addAnnotations(anns)

  override def visitNonLitNodeConstraint(
      ctx: NonLitNodeConstraintContext
  ): Builder[WNodeConstraint] = for {
    nc <- visitInlineNonLitNodeConstraint(ctx.inlineNonLitNodeConstraint())
    // semActs <- visitList(visitSemanticAction, ctx.semanticAction())
    // anns <- visitList(visitAnnotation, ctx.annotation())
  } yield nc // nc.addSemActs(semActs).addAnnotations(anns)

  def visitInlineNonLitNodeConstraint(
      ctx: InlineNonLitNodeConstraintContext
  ): Builder[WNodeConstraint] =
    ctx match {
      /*W      case s: LitNodeConstraintLiteralContext =>
        for {
          nk <- visitNonLiteralKind(s.nonLiteralKind())
          sfs <- visitList(visitStringFacet, s.stringFacet)
          _ <- checkFacets(sfs)
        } yield NodeConstraint.nodeKind(nk, sfs) */

      case s: LitNodeConstraintStringFacetContext =>
        for {
          sfs <- visitList(visitStringFacet, s.stringFacet)
          _ <- checkFacets(sfs)
        } yield WNodeConstraint.xsFacets(sfs)
    }

  def visitInlineShapeAtom(ctx: InlineShapeAtomContext): Builder[WShapeExpr] =
    ctx match {
      case s: InlineShapeAtomNonLitNodeConstraintContext =>
        for {
          nk <- visitInlineNonLitNodeConstraint(s.inlineNonLitNodeConstraint())
          sr <- visitOpt(visitInlineShapeOrRef, s.inlineShapeOrRef())
        } yield sr match {
          case None => nk
          case Some(s) =>
            WShapeAnd(None, List(nk, s)) // W, None, None)
        }
      case s: InlineShapeAtomLitNodeConstraintContext =>
        for {
          se <- visitInlineLitNodeConstraint(s.inlineLitNodeConstraint())
        } yield se
      case s: InlineShapeAtomShapeOrRefContext =>
        for {
          sr <- visitInlineShapeOrRef(s.inlineShapeOrRef())
          nk <- visitOpt(visitInlineNonLitNodeConstraint, s.inlineNonLitNodeConstraint())
        } yield nk match {
          case None    => sr
          case Some(n) => WShapeAnd(None, List(sr, n)) // W, None, None)
        }

      case s: InlineShapeAtomShapeExpressionContext =>
        visitShapeExpression(s.shapeExpression())

      case _: InlineShapeAtomAnyContext =>
        ok(WShapeExpr.any)
      case _ =>
        err(s"Unknown value for inlineShapeAtom ctx: $ctx")
    }

  override def visitValueSet(ctx: ValueSetContext): Builder[List[ValueSetValue]] =
    visitList(visitValueSetValue, ctx.valueSetValue())

  private def nonEmpty[A](ls: java.util.List[A]): Boolean = ls.size > 0

  override def visitValueSetValue(ctx: ValueSetValueContext): Builder[ValueSetValue] =
    ctx match {
      case _ if isDefined(ctx.iriRange()) => visitIriRange(ctx.iriRange())
      /*    case _ if isDefined(ctx.literalRange())  => visitLiteralRange(ctx.literalRange())
      case _ if isDefined(ctx.languageRange()) => visitLanguageRange(ctx.languageRange())
      case _ if nonEmpty(ctx.iriExclusion()) =>
        for {
          exclusions <- visitList(visitIriExclusion, ctx.iriExclusion())
        } yield IRIStemRange(IRIStemWildcard(), Some(exclusions))
      case _ if nonEmpty(ctx.literalExclusion()) =>
        for {
          exclusions <- visitList(visitLiteralExclusion, ctx.literalExclusion())
        } yield LiteralStemRange(LiteralStemRangeWildcard(), Some(exclusions))
      case _ if nonEmpty(ctx.languageExclusion()) =>
        for {
          exclusions <- visitList(visitLanguageExclusion, ctx.languageExclusion())
        } yield LanguageStemRange(LanguageStemRangeWildcard(), Some(exclusions)) */
      case _ => err(s"visitValueSetValue: Unknown value")
    }

  /*  override def visitLiteralRange(ctx: LiteralRangeContext): Builder[ValueSetValue] =
    for {
      literal <- visitLiteral(ctx.literal())
      exclusions <- visitList(visitLiteralExclusion, ctx.literalExclusion)
      str <-
        if (isDefined(ctx.STEM_MARK())) value2String(literal)
        else ok("Unused string") // Unused string...if no STEM_MARK, returns the literal
    } yield
      if (!isDefined(ctx.STEM_MARK()))
        ObjectValue.literalValue(literal)
      else { // it is a stem
        if (exclusions.isEmpty) {
          LiteralStem(str)
        } else
          LiteralStemRange(LiteralStemRangeString(str), Some(exclusions))
      }
   */
  /*  def visitLanguageRange(ctx: LanguageRangeContext): Builder[ValueSetValue] =
    ctx match {
      case s: LanguageRangeFullContext =>
        val lang = getLanguage(s.LANGTAG().getText())
        if (!isDefined(s.STEM_MARK())) {
          ok(Language(lang))
        } else
          for {
            les <- visitList(visitLanguageExclusion, s.languageExclusion())
          } yield
            if (les.isEmpty) LanguageStem(lang)
            else
              LanguageStemRange(LanguageStemRangeLang(lang), Some(les))
      case s: LanguageRangeAtContext =>
        for {
          les <- visitList(visitLanguageExclusion, s.languageExclusion())
        } yield // LanguageStemRange(LanguageStemRangeWildcard(), Some(les))
          if (les.isEmpty) LanguageStem(Lang(""))
          else LanguageStemRange(LanguageStemRangeLang(Lang("")), Some(les))
    } */

  /*  override def visitIriExclusion(ctx: IriExclusionContext): Builder[IRIExclusion] = for {
    iri <- visitIri(ctx.iri())
  } yield
    if (isDefined(ctx.STEM_MARK()))
      IRIStemExclusion(IRIStem(iri))
    else
      IRIRefExclusion(iri) */

  /*  override def visitLanguageExclusion(ctx: LanguageExclusionContext): Builder[LanguageExclusion] = {
    val lang = getLanguage(ctx.LANGTAG().getText())
    ok(
      if (isDefined(ctx.STEM_MARK()))
        LanguageStemExclusion(LanguageStem(lang))
      else LanguageTagExclusion(lang)
    )
  } */

  private def value2String(v: Literal): Builder[String] = ok(v.getLexicalForm)

  /*  override def visitLiteralExclusion(ctx: LiteralExclusionContext): Builder[LiteralExclusion] =
    for {
      literal <- visitLiteral(ctx.literal())
      str <- value2String(literal)
    } yield
      if (isDefined(ctx.STEM_MARK())) {
        LiteralStemExclusion(LiteralStem(str))
      } else LiteralStringExclusion(str)
   */

  def iri2ValueSetValue(iri: IRI): Builder[ValueSetValue] =
    getEntityIRI.flatMap { entityIRI =>
      val itemExp = "Q([0-9]+)".r
      val propertyExp = "P([0-9]+)".r
      val name = iri.str.stripPrefix(entityIRI.str)
      name match {
        case itemExp(ns)    => ok(EntityIdValueSetValue(ItemId(name, iri)))
        case propertyExp(_) => ok(EntityIdValueSetValue(PropertyId(name, iri)))
        case _              => ok(IRIValueSetValue(iri))
      }
    }

  override def visitIriRange(ctx: IriRangeContext): Builder[ValueSetValue] =
    visitIri(ctx.iri()).flatMap(
      iri2ValueSetValue(_)
    )
//W    exclusions <- visitList(visitIriExclusion, ctx.iriExclusion())

  /* WShEx if (!isDefined(ctx.STEM_MARK()))  */

  /* else if (exclusions.isEmpty)
      IRIStem(iri)
    else {
      IRIStemRange(IRIStemValueIRI(iri), Some(exclusions))
    } */

  override def visitLiteral(ctx: LiteralContext): Builder[Literal] =
    ctx match {
      case _ if isDefined(ctx.rdfLiteral()) => visitRdfLiteral(ctx.rdfLiteral())
      case _ if isDefined(ctx.numericLiteral()) =>
        for {
          nl <- visitNumericLiteral(ctx.numericLiteral())
        } yield nl
      case _ if isDefined(ctx.booleanLiteral()) => visitBooleanLiteral(ctx.booleanLiteral())
      case _                                    => err(s"visitLiteral: Unknown ${ctx}")
    }

  override def visitRdfLiteral(ctx: RdfLiteralContext): Builder[Literal] = {
    val str = visitString(ctx.string())
    if (isDefined(ctx.LANGTAG())) {
      val lang = getLanguage(ctx.LANGTAG().getText())
      str.map(s => LangLiteral(s, lang))
    } else if (isDefined(ctx.datatype)) {
      for {
        s <- str
        d <- visitDatatype(ctx.datatype())
      } yield DatatypeLiteral(s, d)
    } else {
      str.map(StringLiteral(_))
    }
  }

  private def okStringLiteral(str: String): Builder[String] =
    ok(unescapeStringLiteral(str))

  override def visitString(ctx: ShExStringContext): Builder[String] =
    if (isDefined(ctx.STRING_LITERAL_LONG1())) {
      okStringLiteral(stripStringLiteralLong1(ctx.STRING_LITERAL_LONG1().getText()))
    } else if (isDefined(ctx.STRING_LITERAL_LONG2())) {
      okStringLiteral(stripStringLiteralLong2(ctx.STRING_LITERAL_LONG2().getText()))
    } else if (isDefined(ctx.STRING_LITERAL1())) {
      okStringLiteral(stripStringLiteral1(ctx.STRING_LITERAL1().getText()))
    } else if (isDefined(ctx.STRING_LITERAL2())) {
      okStringLiteral(stripStringLiteral2(ctx.STRING_LITERAL2().getText()))
    } else
      err(s"visitString: Unknown ctx ${ctx.getClass.getName}")

  def stripStringLiteral1(s: String): String = {
    val regexStr = "\'(.*)\'".r
    s match {
      case regexStr(s) => s
      case _           => throw new Exception(s"stripStringLiteral2 $s doesn't match regex")
    }
  }

  def stripStringLiteral2(s: String): String = {
    val regexStr = "\"(.*)\"".r
    s match {
      case regexStr(s) => s
      case _           => throw new Exception(s"stripStringLiteral2 $s doesn't match regex")
    }
  }

  def stripStringLiteralLong1(s: String): String = {
    val regexStr = "\'\'\'(.*)\'\'\'".r
    s match {
      case regexStr(s) => s
      case _           => throw new Exception(s"stripStringLiteralLong1 $s doesn't match regex")
    }
  }

  def stripStringLiteralLong2(s: String): String = {
    val regexStr = "\"\"\"(.*)\"\"\"".r
    s match {
      case regexStr(s) => s
      case _           => throw new Exception(s"stripStringLiteralLong1 $s doesn't match regex")
    }
  }

  override def visitDatatype(ctx: DatatypeContext): Builder[IRI] =
    visitIri(ctx.iri())

  override def visitIri(ctx: IriContext): Builder[IRI] =
    if (isDefined(ctx.IRIREF())) for {
      base <- getBase
      iri <- extractIRIfromIRIREF(ctx.IRIREF().getText, base)
    } yield iri
    else
      for {
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
      }
    )
  }

  def splitPrefix(str: String): (String, String) =
    if (str contains ':') {
      val (prefix, name) = str.splitAt(str.lastIndexOf(':'))
      (prefix, name.tail)
    } else {
      ("", str)
    }

  override def visitPrefixedName(ctx: PrefixedNameContext): Builder[String] =
    ok(ctx.getText())
    /*    ctx match {
      case _ if isDefined(ctx.PNAME_LN()) => ok(ctx.PNAME_LN().getText())
      case _ if isDefined(ctx.PNAME_NS()) => ok(ctx.PNAME_NS().getText())
      case _ => err("visitPrefixedName: Unknown value")
    } */

  /*   override def visitNodeConstraintGroup(ctx: NodeConstraintGroupContext): Builder[ShapeExpr] =
       for {
        shapeOrRef <- visitShapeOrRef(ctx.shapeOrRef())
      } yield shapeOrRef */

  def extractIRIfromIRIREF(d: String, base: Option[IRI]): Builder[IRI] = {
    val str = unescapeIRI(d)
    val iriRef = "^<(.*)>$".r
    str match {
      case iriRef(i) =>
        IRI
          .fromString(i, base)
          .fold(
            str => err(str),
            i =>
              base match {
                case None => ok(i)
                case Some(b) =>
                  if (b.uri.toASCIIString.startsWith("file:///")) {
                    // For some reason, when resolving a file:///foo iri, the system returns file:/foo
                    // The following code keeps the file:/// part
                    ok(IRI(b.uri.resolve(i.uri).toASCIIString.replaceFirst("file:/", "file:///")))
                  } else {
                    ok(IRI(b.uri.resolve(i.uri)))
                  }
              }
          )
      case s => err(s"IRIREF: $s does not match <...>")
    }
  }

  override def visitNumericLiteral(ctx: NumericLiteralContext): Builder[Literal] =
    ctx match {
      case _ if isDefined(ctx.INTEGER()) =>
        val repr = ctx.INTEGER().getText
        ok(IntegerLiteral(Integer.parseInt(repr), repr))
      case _ if isDefined(ctx.DECIMAL()) =>
        val repr = ctx.DECIMAL().getText
        ok(DecimalLiteral(BigDecimal(repr), repr))
      case _ if isDefined(ctx.DOUBLE()) =>
        val repr = ctx.DOUBLE().getText
        ok(DoubleLiteral(repr.toDouble, repr))
      case _ => err("Unknown ctx in numericLiteral")
    }

  def getBigInt(str: String): Builder[BigInt] =
    try
      ok(BigInt(str))
    catch {
      case _: Throwable => err(s"Cannot get BigInt from $str")
    }

  def getInteger(str: String): Builder[Int] =
    try
      ok(str.toInt)
    catch {
      case _: NumberFormatException =>
        err(s"Cannot get integer from $str")
    }

  def getDecimal(str: String): Builder[BigDecimal] =
    try
      ok(BigDecimal(str))
    catch {
      case _: NumberFormatException =>
        err(s"Cannot get decimal from $str")
    }

  def getDouble(str: String): Builder[Double] =
    try
      ok(str.toDouble)
    catch {
      case _: NumberFormatException =>
        err(s"Cannot get double from $str")
    }

  override def visitBooleanLiteral(ctx: BooleanLiteralContext): Builder[Literal] =
    if (isDefined(ctx.KW_TRUE()))
      ok(BooleanLiteral(true))
    else
      ok(BooleanLiteral(false))

  /*  override def visitXsFacet(ctx: XsFacetContext): Builder[XsFacet] =
    ctx match {
      case _ if isDefined(ctx.stringFacet()) =>
        visitStringFacet(ctx.stringFacet())
      case _ if isDefined(ctx.numericFacet()) =>
        visitNumericFacet(ctx.numericFacet())
      case _ => err(s"visitXsFacet: Unsupported ${ctx.getClass.getName}")
    }
   */

  override def visitStringSet(ctx: StringSetContext): Builder[StringSet] =
    visitList(visitString, ctx.string()).map(ss => StringSet(ss))

  override def visitStringFacet(ctx: StringFacetContext): Builder[StringFacet] = ctx match {
    case _ if isDefined(ctx.stringLength()) =>
      for {
        n <- getInteger(ctx.INTEGER().getText())
        stringLength <- visitStringLength(ctx.stringLength)(n)
      } yield stringLength
    case _ if isDefined(ctx.REGEXP()) =>
      ok(
        Pattern(
          unescapePattern(removeSlashes(ctx.REGEXP().getText())),
          if (isDefined(ctx.REGEXP_FLAGS())) Some(ctx.REGEXP_FLAGS().getText())
          else None
        )
      )
    /*    case _ if (isDefined(ctx.KW_PATTERN())) => for {
      str <- visitString(ctx.string())
    } yield Pattern(str, None)  */
    case _ => err(s"visitStringFacet: Unsupported ${ctx.getClass.getName}")
  }

  private def unscapeSlashes(str: String): String =
    // str.replaceAllLiterally(s"\\/", "/")
    str.replace(s"\\/", "/")

  private def removeSlashes(str: String): String = {
    val slashedRegex = "/(.*)/".r
    str match {
      case slashedRegex(s) => unscapeSlashes(s)
      case _               => str
    }
  }

  override def visitStringLength(ctx: StringLengthContext): Int => Builder[StringFacet] = n =>
    if (isDefined(ctx.KW_LENGTH())) ok(Length(n))
    else if (isDefined(ctx.KW_MINLENGTH())) ok(MinLength(n))
    else if (isDefined(ctx.KW_MAXLENGTH())) ok(MaxLength(n))
    else
      err(s"visitStringLength: Unknown value for $ctx")

  sealed trait NumericRange
  case object NRMinInclusive extends NumericRange
  case object NRMinExclusive extends NumericRange
  case object NRMaxInclusive extends NumericRange
  case object NRMaxExclusive extends NumericRange

  sealed trait NumericLength
  case object NLTotalDigits extends NumericLength
  case object NLFractionDigits extends NumericLength

  override def visitNumericFacet(ctx: NumericFacetContext): Builder[XsFacet] =
    ctx match {
      case _ if isDefined(ctx.numericRange()) =>
        for {
          nr <- visitNumericRange(ctx.numericRange())
          v <- visitRawNumeric(ctx.rawNumeric())
          nf <- makeNumericFacet(nr, v)
        } yield nf
      case _ if isDefined(ctx.numericLength()) =>
        for {
          nl <- visitNumericLength(ctx.numericLength())
          n <- getInteger(ctx.INTEGER().getText)
          numLength <- makeNumericLength(nl, n)
        } yield numLength
      case _ => err("VisitNumericFacet. Unknown state for ctx")
    }

  override def visitRawNumeric(ctx: RawNumericContext): Builder[NumericLiteral] =
    ctx match {
      case _ if isDefined(ctx.INTEGER()) =>
        val repr = ctx.INTEGER().getText
        ok(NumericInt(Integer.parseInt(repr), repr))
      case _ if isDefined(ctx.DECIMAL()) =>
        val repr = ctx.DECIMAL().getText
        ok(NumericDecimal(BigDecimal(repr), repr))
      case _ if isDefined(ctx.DOUBLE()) =>
        val repr = ctx.DOUBLE().getText
        ok(NumericDouble(repr.toDouble, repr))
      case _ => err("Unknown ctx in numericLiteral")
    }

  override def visitNumericRange(ctx: NumericRangeContext): Builder[NumericRange] =
    ctx match {
      case _ if isDefined(ctx.KW_MININCLUSIVE()) => ok(NRMinInclusive)
      case _ if isDefined(ctx.KW_MINEXCLUSIVE()) => ok(NRMinExclusive)
      case _ if isDefined(ctx.KW_MAXINCLUSIVE()) => ok(NRMaxInclusive)
      case _ if isDefined(ctx.KW_MAXEXCLUSIVE()) => ok(NRMaxExclusive)
    }

  override def visitNumericLength(ctx: NumericLengthContext): Builder[NumericLength] =
    ctx match {
      case _ if isDefined(ctx.KW_TOTALDIGITS())    => ok(NLTotalDigits)
      case _ if isDefined(ctx.KW_FRACTIONDIGITS()) => ok(NLFractionDigits)
    }

  def makeNumericLiteral(lexicalForm: String, datatype: IRI): Builder[NumericLiteral] =
    datatype match {
      case `xsd:integer` =>
        for {
          n <- getInteger(lexicalForm)
        } yield NumericInt(n, lexicalForm)
      case `xsd:decimal` =>
        for {
          d <- getDecimal(lexicalForm)
        } yield NumericDecimal(d, lexicalForm)
      case `xsd:double` =>
        for {
          d <- getDouble(lexicalForm)
        } yield NumericDouble(d, lexicalForm)
      case `xsd:float` =>
        for { // TODO: Check if floats and doubles are equivalent
          d <- getDouble(lexicalForm)
        } yield NumericDouble(d, lexicalForm)
      case _ => err(s"Numeric Literal '$lexicalForm' applied to unknown datatype $datatype ")
    }

  def makeNumericFacet(nr: NumericRange, nl: NumericLiteral): Builder[NumericFacet] =
    nr match {
      case NRMinInclusive => ok(MinInclusive(nl))
      case NRMinExclusive => ok(MinExclusive(nl))
      case NRMaxInclusive => ok(MaxInclusive(nl))
      case NRMaxExclusive => ok(MaxExclusive(nl))
    }

  def makeNumericLength(nr: NumericLength, n: Int): Builder[NumericFacet] =
    nr match {
      case NLTotalDigits    => ok(TotalDigits(n))
      case NLFractionDigits => ok(FractionDigits(n))
    }

  /*W  override def visitNodeConstraintLiteral(ctx: NodeConstraintLiteralContext): Builder[ShapeExpr] =
    for {
      facets <- visitList(visitXsFacet, ctx.xsFacet())
      _ <- checkFacets(facets)
    } yield NodeConstraint.nodeKind(LiteralKind, facets) */

  // TODO: Check there are non repeated facets
  def checkFacets(ls: List[XsFacet]): Builder[Unit] = {
    case class Status(visited: List[XsFacet], errors: List[String])
    val zero = Status(List(), List())
    def cmb(facet: XsFacet, status: Status): Status = {
      val sameTypeFacets = status.visited.filter(_.sameTypeAs(facet))
      if (sameTypeFacets.isEmpty) status.copy(visited = facet +: status.visited)
      else
        status.copy(
          errors =
            s"Facets with same type as ${facet.toString}. ${sameTypeFacets.map(_.toString).mkString(",")}" +: status.errors
        )
    }
    val result = ls.foldRight(zero)(cmb)
    if (result.errors.isEmpty) ok(())
    else err(s"Error checking facets: ${result.errors.mkString("\n")}")
  }
  // ok(())

  /*  override def visitNodeConstraintNonLiteral(
      ctx: NodeConstraintNonLiteralContext
  ): Builder[ShapeExpr] = for {
    nk <- visitNonLiteralKind(ctx.nonLiteralKind())
    facets <- visitList(visitStringFacet, ctx.stringFacet())
    _ <- checkFacets(facets)
  } yield NodeConstraint.nodeKind(nk, facets)
   */

  /*  override def visitNonLiteralKind(ctx: NonLiteralKindContext): Builder[NodeKind] =
    ctx match {
      case _ if isDefined(ctx.KW_IRI())        => ok(IRIKind)
      case _ if isDefined(ctx.KW_BNODE())      => ok(BNodeKind)
      case _ if isDefined(ctx.KW_NONLITERAL()) => ok(NonLiteralKind)
    } */
  override def visitInlineShapeOrRef(ctx: InlineShapeOrRefContext): Builder[WShapeExpr] =
    ctx match {
      case _ if isDefined(ctx.inlineShapeDefinition()) =>
        visitInlineShapeDefinition(ctx.inlineShapeDefinition())
      case _ if isDefined(ctx.shapeRef()) =>
        visitShapeRef(ctx.shapeRef()).map(WShapeRef(None, _)) // , None, None))
      case _ => err(s"internal Error: visitShapeOrRef. Unknown $ctx")
    }

  override def visitShapeRef(ctx: ShapeRefContext): Builder[ShapeLabel] = ctx match {
    case _ if isDefined(ctx.ATPNAME_NS()) =>
      val nameNS = ctx.ATPNAME_NS().getText().tail
      resolve(nameNS).map(iri => IRILabel(iri))
    case _ if isDefined(ctx.ATPNAME_LN()) =>
      val nameLN = ctx.ATPNAME_LN().getText().tail
      resolve(nameLN).map(iri => IRILabel(iri))
    case _ if isDefined(ctx.shapeExprLabel()) =>
      for {
        lbl <- visitShapeExprLabel(ctx.shapeExprLabel())
      } yield lbl
  }

  override def visitShapeOrRef(ctx: ShapeOrRefContext): Builder[WShapeExpr] = ctx match {
    case _ if isDefined(ctx.shapeDefinition()) =>
      visitShapeDefinition(ctx.shapeDefinition())
    case _ if isDefined(ctx.shapeRef()) =>
      visitShapeRef(ctx.shapeRef())
        .map(lbl => WShapeRef(None, lbl)) // , None, None))
    case _ => err(s"internal Error: visitShapeOrRef. Unknown $ctx")
  }

  override def visitInlineShapeDefinition(ctx: InlineShapeDefinitionContext): Builder[WShapeExpr] =
    for {
      qualifiers <- visitList(visitQualifier, ctx.qualifier())
      tripleExpr <- visitOpt(visitTripleExpression, ctx.tripleExpression)
      shape <-
        makeShape(qualifiers, tripleExpr) // W , List(), List())
    } yield shape

  override def visitTripleExpression(ctx: TripleExpressionContext): Builder[TripleExpr] =
    visitOneOfTripleExpr(ctx.oneOfTripleExpr())

  override def visitShapeDefinition(ctx: ShapeDefinitionContext): Builder[WShapeExpr] =
    for {
      se <- visitInlineShapeDefinition(ctx.inlineShapeDefinition)
//W      semActs <- visitList(visitSemanticAction, ctx.semanticAction())
//W      anns <- visitList(visitAnnotation, ctx.annotation())
    } yield se // W .addSemActs(semActs).addAnnotations(anns)

  // TODO: Maybe remove the following method
  /*  def addAnnotations(
      maybeTe: Option[TripleExpr],
      anns: List[Annotation]
  ): Builder[Option[TripleExpr]] =
    maybeTe match {
      case None => ok(None)
      case Some(te) =>
        if (anns.isEmpty) ok(maybeTe)
        else
          te match {
            case t: TripleConstraint => ok(Some(t.copy(annotations = Some(anns))))
            case so: OneOf           => ok(Some(so.copy(annotations = Some(anns))))
            case eo: EachOf          => ok(Some(eo.copy(annotations = Some(anns))))
            case _                   => err(s"Can't add annotations $anns to $te")
          }
    } */

  def makeShape(
      qualifiers: List[Qualifier],
      tripleExpr: Option[TripleExpr]
      // semActs: List[SemAct],
      // anns: List[Annotation]
  ): Builder[WShapeExpr] = {
    val containsClosed =
      if (qualifiers.contains(Closed))
        true
      else
        false
    val tcs: List[TermConstraint] = qualifiers.map(_.getTermConstraints).flatten
    val es: List[PropertyId] = qualifiers.map(_.getExtras).flatten
    // W val inheritList = qualifiers.map(_.getExtends).flatten
    // W val restrictsList = qualifiers.map(_.getRestricts).flatten
    val shape = WShape.empty
      .withClosed(if (qualifiers.isEmpty) false else containsClosed)
      .withExpression(tripleExpr)
      .withExtras(es)
      .withTermConstraints(tcs)
    ok(shape)
  }

  override def visitQualifier(ctx: QualifierContext): Builder[Qualifier] =
    ctx match {
      case _ if isDefined(ctx.KW_CLOSED()) => ok(Closed)
      case _ if isDefined(ctx.extension()) =>
        visitExtension(ctx.extension())
      case _ if isDefined(ctx.restriction()) =>
        visitRestriction(ctx.restriction())
      case _ if isDefined(ctx.extraPropertySet()) =>
        visitExtraPropertySet(ctx.extraPropertySet())
      case _ if isDefined(ctx.labelConstraint()) =>
        visitLabelConstraint(ctx.labelConstraint())
      case _ if isDefined(ctx.descriptionConstraint()) =>
        visitDescriptionConstraint(ctx.descriptionConstraint())
      case _ if isDefined(ctx.aliasConstraint()) =>
        visitAliasConstraint(ctx.aliasConstraint())
    }

  override def visitExtension(ctx: ExtensionContext): Builder[Qualifier] = for {
    sl <- visitList(visitShapeRef, ctx.shapeRef())
  } yield Extends(sl)

  override def visitRestriction(ctx: RestrictionContext): Builder[Qualifier] = for {
    sl <- visitList(visitShapeRef, ctx.shapeRef())
  } yield Restricts(sl)

  override def visitExtraPropertySet(ctx: ExtraPropertySetContext): Builder[Qualifier] = for {
    preds <- visitList(visitPredicate, ctx.predicate())
    ps <- preds.map(predicate2PropertyId(_)).sequence
  } yield Extra(ps)

  override def visitLabelConstraint(ctx: LabelConstraintContext): Builder[Qualifier] =
    visitLangConstraints(ctx.langConstraints()).flatMap { cs =>
      ok(TermConstraintQ(cs.map { case (lang, cs) => 
        LabelConstraint(Map(lang -> cs)) }))
    }

  override def visitDescriptionConstraint(ctx: DescriptionConstraintContext): Builder[Qualifier] =
    visitLangConstraints(ctx.langConstraints()).flatMap { cs =>
      ok(TermConstraintQ(cs.map { case (lang, cs) => 
        DescriptionConstraint(Map(lang -> cs)) }))
    }

  override def visitAliasConstraint(ctx: AliasConstraintContext): Builder[Qualifier] =
    visitLangConstraints(ctx.langConstraints()).flatMap { cs =>
      ok(TermConstraintQ(cs.map { case (lang, cs) => 
        AliasConstraint(Map(lang -> cs)) 
      }))
    }

  override def visitLangConstraints(
      ctx: LangConstraintsContext
  ): Builder[List[(Lang, Option[StringConstraint])]] =
    ctx match {
      case _ if isDefined(ctx.singleLangConstraint()) =>
        visitSingleLangConstraint(ctx.singleLangConstraint())
          .map(List(_))
      case _ if isDefined(ctx.multiLangConstraint()) =>
        visitMultiLangConstraint(ctx.multiLangConstraint())
    }

  override def visitSingleLangConstraint(
      ctx: SingleLangConstraintContext
  ): Builder[(Lang, Option[StringConstraint])] =
    visitLangConstraint(ctx.langConstraint())

  override def visitMultiLangConstraint(
      ctx: MultiLangConstraintContext
  ): Builder[List[(Lang, Option[StringConstraint])]] =
    visitList(visitLangConstraint, ctx.langConstraint())

  override def visitLangConstraint(
      ctx: LangConstraintContext
  ): Builder[(Lang, Option[StringConstraint])] = for {
    sc <- visitStringConstraint(ctx.stringConstraint())
  } yield {
    val lang = getLanguage(ctx.LANGLABEL().getText())
    (lang, sc)
  }

  override def visitStringConstraint(
      ctx: StringConstraintContext
  ): Builder[Option[StringConstraint]] = ctx match {
    case _ if isDefined(ctx.any()) =>
      ok(None)
    case _ if isDefined(ctx.stringSet()) =>
      visitStringSet(ctx.stringSet())
        .map(_.some)
    case _ if isDefined(ctx.stringFacet()) =>
      visitStringFacet(ctx.stringFacet())
        .map(Facet(_).some)
  }

  override def visitOneOfTripleExpr(ctx: OneOfTripleExprContext): Builder[TripleExpr] = ctx match {
    case _ if isDefined(ctx.groupTripleExpr()) =>
      for {
        tripleExpr <- visitGroupTripleExpr(ctx.groupTripleExpr())
      } yield tripleExpr
    case _ if isDefined(ctx.multiElementOneOf()) =>
      for {
        tripleExpr <- visitMultiElementOneOf(ctx.multiElementOneOf())
      } yield tripleExpr
    case _ => err(s"visitOneOfShape: unknown $ctx")
  }

  override def visitGroupTripleExpr(ctx: GroupTripleExprContext): Builder[TripleExpr] =
    ctx match {
      case _ if isDefined(ctx.singleElementGroup()) =>
        visitSingleElementGroup(ctx.singleElementGroup())
      case _ if isDefined(ctx.multiElementGroup()) =>
        visitMultiElementGroup(ctx.multiElementGroup())
      case _ => err(s"visitGroupTripleExpr: unknown $ctx")
    }

  override def visitUnaryTripleExpr(ctx: UnaryTripleExprContext): Builder[TripleConstraint] =
    ctx match {
      /*W TODO     case _ if isDefined(ctx.bracketedTripleExpr()) => visitBracketedTripleExpr(ctx.bracketedTripleExpr()) */
      case _ if isDefined(ctx.tripleConstraint()) => visitTripleConstraint(ctx.tripleConstraint())
      case _                                      => err(s"visitUnaryTripleExpr: Unknown ctx: $ctx")
    }

  /*W    ctx match {
      case _ if isDefined(ctx.include()) =>
        visitInclude(ctx.include())
      case _ if isDefined(ctx.expr()) =>
        for {
          e <- visitExpr(ctx.expr())
        } yield Expr(None, e)
      case _ =>
        for {
          maybeLbl <- visitOpt(visitTripleExprLabel, ctx.tripleExprLabel())
          te <-
            if (isDefined(ctx.bracketedTripleExpr()))
              visitBracketedTripleExpr(ctx.bracketedTripleExpr())
            else if (isDefined(ctx.tripleConstraint()))
              visitTripleConstraint(ctx.tripleConstraint())
            else err[TripleExpr](s"visitUnaryTripleExpr: unknown $ctx")
           te1 <- maybeLbl match {
            case None      => ok(te)
            case Some(lbl) => addTripleExprLabel(lbl, te)
          }
      } yield te1
  override def visitExpr(e: ExprContext): Builder[ValueExpr] = e match {
    case _ if isDefined(e.basicExpr()) =>
      for {
        e <- visitBasicExpr(e.basicExpr())
      } yield e
    case _ if isDefined(e.binOp()) =>
      for {
        e1 <- visitExpr(e.expr(0))
        op <- visitBinOp(e.binOp())
        e2 <- visitExpr(e.expr(1))
      } yield BinExpr(e1, op, e2)
  } */

  override def visitBasicExpr(e: BasicExprContext): Builder[ValueExpr] = e match {
    // case _ if (isDefined(e.varName())) => {
    //   ok(Var(VarName(e.varName().getText)))
    // }
    case _ if isDefined(e.literal()) =>
      for {
        literal <- visitLiteral(e.literal())
      } yield Const(literal)
    case _ if isDefined(e.iri())       => err("Not implemented iris yet in visitBasicExpr")
    case _ if isDefined(e.blankNode()) => err("Not implemented blankNode yet in visitBasicExpr")
  }

  /*  private def visitBinOp(b: BinOpContext): Builder[BinOp] = b match {
    case _: EqualsContext    => ok(Equals)
    case _: NotEqualsContext => ok(NotEquals)
    case _: AddContext       => ok(Add)
    case _: MinusContext     => ok(Minus)
    case _: DivContext       => ok(Div)
    case _: MultContext      => ok(Mul)
    case _: LeContext        => ok(LE)
    case _: GeContext        => ok(GE)
    case _: LtContext        => ok(LT)
    case _: GtContext        => ok(GT)
  } */

  // TODO: Where should I put ProductionLabels ?
  override def visitTripleExprLabel(ctx: TripleExprLabelContext): Builder[ShapeLabel] = ctx match {
    case _ if isDefined(ctx.iri())       => visitIri(ctx.iri()).map(IRILabel(_))
    case _ if isDefined(ctx.blankNode()) => visitBlankNode(ctx.blankNode()).map(BNodeLabel(_))
    case _                               => err(s"Unknown tripelExprLabel")
  }

  // override def visitVariableDecl(ctx: VariableDeclContext):Builder[VarName] = {
  //   ok(VarName(ctx.varName().getText))
  // }

  private def extractProperty(entityIRI: IRI, predicate: IRI): Builder[PropertyId] = {
    val p = "P[0-9]+".r
    p.findFirstIn(predicate.str.stripPrefix(entityIRI.str)) match {
      case Some(name) => ok(PropertyId(name, predicate))
      case _ =>
        err(s"extractProperty: Predicate $predicate doesn't match entityIRI $entityIRI + P[0-9]+")
    }
  }

  private def predicate2PropertyId(predicate: IRI): Builder[PropertyId] =
    getEntityIRI.flatMap(extractProperty(_, predicate))

  override def visitTripleConstraint(ctx: TripleConstraintContext): Builder[TripleConstraint] =
    for {
//W      sense <- visitSenseFlags(ctx.senseFlags())
      predicate <- visitPredicate(ctx.predicate())
      shapeExpr <- visitInlineShapeExpression(ctx.inlineShapeExpression())
      cardinality <- getCardinality(ctx.cardinality())
      qs <- getPropertySpec(ctx.propertySpec()).map(_.map(QualifierSpec(_, false)))
      refs <- getReferencesSpec(ctx.referencesSpec())
      min = cardinality.min
      max = cardinality.max
      // W anns <- visitList(visitAnnotation, ctx.annotation())
      // W semActs <- visitList(visitSemanticAction, ctx.semanticAction())
      propertyId <- predicate2PropertyId(predicate)
      tc <- shapeExpr match {
        case sref: WShapeRef =>
          ok(tripleConstraintRef(propertyId, sref, min, max).withQs(qs).withRefs(refs))
        case nc: WNodeConstraint =>
          ok(tripleConstraintLocal(propertyId, nc, min, max).withQs(qs).withRefs(refs))
        case WShape(None, false, Nil, None, Nil) =>
          ok(tripleConstraintLocal(propertyId, emptyExpr, min, max).withQs(qs).withRefs(refs))
        case se: WShapeExpr =>
          ok(TripleConstraintGeneral(propertyId, se, min, max).withQs(qs).withRefs(refs))
      }
    } yield tc

  private def getReferencesSpec(ctx: ReferencesSpecContext): Builder[Option[ReferencesSpec]] =
    if (isDefined(ctx)) {
      visitOneOfReferencesExpr(ctx.oneOfReferencesExpr()).map(_.some)
    } else ok(none)

  override def visitOneOfReferencesExpr(ctx: OneOfReferencesExprContext): Builder[ReferencesSpec] =
    visitSingleReferencesExpr(ctx.singleReferencesExpr()).flatMap(single =>
      visitList(visitOneOfReferencesExpr, ctx.oneOfReferencesExpr()).flatMap(ls =>
        if (ls.isEmpty) ok(single)
        else ok(ReferencesOneOf(single +: ls))
      )
    )

  override def visitSingleReferencesExpr(
      ctx: SingleReferencesExprContext
  ): Builder[ReferencesSpec] =
    visitPropertySpec(ctx.propertySpec()).flatMap(propertySpec =>
      getCardinality(ctx.cardinality()).flatMap(cardinality =>
        ok(ReferencesSpecSingle(propertySpec, cardinality.min, cardinality.max, false))
      )
    )

  override def visitPropertySpec(ctx: PropertySpecContext): Builder[PropertySpec] =
    visitOneOfPropertyExpr(ctx.oneOfPropertyExpr())

  override def visitOneOfPropertyExpr(ctx: OneOfPropertyExprContext): Builder[PropertySpec] =
    visitEachOfPropertyExpr(ctx.eachOfPropertyExpr()).flatMap(eo =>
      visitList(visitOneOfPropertyExpr, ctx.oneOfPropertyExpr()).flatMap(rest =>
        ok(if (rest.isEmpty) eo else OneOfPs(eo +: rest, 1, IntLimit(1)))
      )
    )

  override def visitEachOfPropertyExpr(ctx: EachOfPropertyExprContext): Builder[PropertySpec] =
    visitSinglePropertyExpr(ctx.singlePropertyExpr()).flatMap(single =>
      visitList(visitEachOfPropertyExpr, ctx.eachOfPropertyExpr()).flatMap(rest =>
        ok(if (rest.isEmpty) single else EachOfPs(single +: rest, 1, IntLimit(1)))
      )
    )

  override def visitSinglePropertyExpr(ctx: SinglePropertyExprContext): Builder[PropertySpec] =
    visitPredicate(ctx.predicate()).flatMap(pred =>
      predicate2PropertyId(pred).flatMap(propId =>
        visitShapeAtom(ctx.shapeAtom()).flatMap(se =>
          getCardinality(ctx.cardinality()).flatMap(cardinality =>
            se match {
              case nc: WNodeConstraint =>
                ok(PropertyLocal(propId, nc, cardinality.min, cardinality.max))
              case sref: WShapeRef =>
                ok(PropertyRef(propId, sref, cardinality.min, cardinality.max))
              case WShape(None, false, Nil, None, Nil) =>
                ok(PropertyLocal(propId, emptyExpr, cardinality.min, cardinality.max))
              case _ => err(s"getPropertySpec. Error matching shapeExpr: $se")
            }
          )
        )
      )
    )

  private def getPropertySpec(ctx: PropertySpecContext): Builder[Option[PropertySpec]] =
    if (isDefined(ctx)) {
      visitPropertySpec(ctx).map(_.some)
    } else ok(none)

  /*  case class Sense(optInverse: Option[Boolean], optNegated: Option[Boolean])

  override def visitSenseFlags(ctx: SenseFlagsContext): Builder[Sense] =
    if (isDefined(ctx)) {
      val ls: List[String] = ctx.children.asScala.toList.map(_.getText)
      val optInverse = if (ls.contains("^")) Some(true) else None
      val optNegated = if (ls.contains("!")) Some(true) else None
      ok(Sense(optInverse, optNegated))
    } else ok(Sense(None, None))
   */
  def getCardinality(ctx: CardinalityContext): Builder[Cardinality] =
    if (isDefined(ctx))
      ctx match {
        case _: StarCardinalityContext     => ok(star)
        case _: PlusCardinalityContext     => ok(plus)
        case _: OptionalCardinalityContext => ok(optional)
        case s: RepeatCardinalityContext   => visitRepeatCardinality(s)
        case _ => err(s"Not implemented cardinality ${ctx.getClass.getName}")
      }
    else ok(defaultCardinality)

  override def visitRepeatCardinality(ctx: RepeatCardinalityContext): Builder[Cardinality] =
    visitRepeatRange(ctx.repeatRange())

  private def visitRepeatRange(ctx: RepeatRangeContext): Builder[Cardinality] =
    ctx match {
      case s: ExactRangeContext =>
        getInteger(s.INTEGER().getText()).map(n => Cardinality(n, IntLimit(n)))
      case s: MinMaxRangeContext =>
        for {
          min <- visitMin_range(s.min_range())
          max <- visitMax_range(s.max_range())
        } yield Cardinality(min, max)
      case _ => err(s"visitRepeatRange: unknown value of ctx: ${ctx.getClass().getName()}")
    }

  override def visitMin_range(ctx: Min_rangeContext): Builder[Int] =
    getInteger(ctx.INTEGER().getText())

  override def visitMax_range(ctx: Max_rangeContext): Builder[IntOrUnbounded] =
    if (isDefined(ctx)) {
      if (isDefined(ctx.INTEGER())) {
        getInteger(ctx.INTEGER().getText()).map(n => IntLimit(n))
      } else {
        // Asume star
        ok(Unbounded)
      }
    } else
      ok(IntLimit(1)) // W TODO: Not sure about this

  override def visitPredicate(ctx: PredicateContext): Builder[IRI] =
    ctx match {
      case _ if isDefined(ctx.iri()) =>
        visitIri(ctx.iri())
      /*W      case _ if isDefined(ctx.rdfType()) =>
        ok(`rdf:type`) */
      case _ => err(s"visitPredicate: Unknown value of ctx ${ctx.getClass.getName}")
    }

  /*W  override def visitInclude(ctx: IncludeContext): Builder[TripleExpr] =
    for {
      lbl <- visitTripleExprLabel(ctx.tripleExprLabel())
    } yield Inclusion(lbl) */

  override def visitBracketedTripleExpr(ctx: BracketedTripleExprContext): Builder[TripleExpr] =
    for {
      tripleExpr <- visitTripleExpression(ctx.tripleExpression())
      cardinality <- getCardinality(ctx.cardinality())
//W      annotations <- visitList(visitAnnotation, ctx.annotation())
//W      semActs <- visitList(visitSemanticAction, ctx.semanticAction())
    } yield extendTripleExpr(tripleExpr, cardinality) // W , annotations, semActs)

  def extendTripleExpr(
      te: TripleExpr,
      cardinality: Cardinality
      // W anns: List[Annotation],
      // W sActs: List[SemAct]
  ): TripleExpr =
    te match {
      case tc: TripleConstraintLocal =>
        tc.copy(
          min = cardinality.min,
          max = cardinality.max
          // W annotations = optListCombine(tc.annotations, anns),
          // W semActs = optListCombine(tc.semActs, sActs)
        )
      case tc: TripleConstraintRef =>
        tc.copy(
          min = cardinality.min,
          max = cardinality.max
          // W annotations = optListCombine(tc.annotations, anns),
          // W semActs = optListCombine(tc.semActs, sActs)
        )
      // case et: EmptyTripleExpr => ???
      case tg: TripleConstraintGeneral =>
        tg.copy(
          min = cardinality.min,
          max = cardinality.max
          // W annotations = optListCombine(tc.annotations, anns),
          // W semActs = optListCombine(tc.semActs, sActs)
        )
      case eo: EachOf =>
        eo /*W .copy(
          min = cardinality._1,
          max = cardinality._2,
          //W annotations = optListCombine(eo.annotations, anns),
          //W semActs = optListCombine(eo.semActs, sActs)
        ) */
      case so: OneOf =>
        so /*W .copy(
          min = cardinality._1,
          max = cardinality._2,
          //W annotations = optListCombine(so.annotations, anns),
          //W semActs = optListCombine(so.semActs, sActs)
        ) */
      /*W      case i: Inclusion =>
        // TODO: Check how to extend include
        i */
      /*W      case e: Expr =>
        // TODO: Check how to extend include
        e */
      case EmptyTripleExpr =>
        throw new RuntimeException(s"Cannot extend emptyTripleExpr with cardinality $cardinality")

    }

  def optListCombine[A](maybeAs: Option[List[A]], as: List[A]): Option[List[A]] =
    maybeAs match {
      case None =>
        if (as.isEmpty) None
        else Some(as)
      case Some(as1) => Some(as1 ++ as)
    }

  /*W  override def visitAnnotation(ctx: AnnotationContext): Builder[Annotation] = for {
    pred <- visitPredicate(ctx.predicate())
    obj <-
      if (isDefined(ctx.iri())) {
        visitIri(ctx.iri()).map { iri =>
          val o: ObjectValue = IRIValue(iri)
          o
        }
      } else {
        visitLiteral(ctx.literal()).map(v =>
          v match {
            case o: Literal => ObjectValue.literalValue(o)
            case _          => throw new Exception(s"Unknown value in annotation $v")
          }
        )
      }
  } yield Annotation(pred, obj) */

  /*  override def visitInnerTripleExpr(ctx: InnerTripleExprContext): Builder[TripleExpr] =
    ctx match {
      case _ if isDefined(ctx.multiElementGroup()) => visitMultiElementGroup(ctx.multiElementGroup())
      case _ if isDefined(ctx.multiElementOneOf()) => visitMultiElementOneOf(ctx.multiElementOneOf())
      case _ => err("visitInnerShape. Unknown alternative")
    } */

  override def visitSingleElementGroup(ctx: SingleElementGroupContext): Builder[TripleExpr] =
    visitUnaryTripleExpr(ctx.unaryTripleExpr()).flatMap { tc =>
      val te: TripleExpr = tc
      ok(te)
    }

  override def visitMultiElementGroup(ctx: MultiElementGroupContext): Builder[TripleExpr] =
    for {
      ses <- visitList(visitUnaryTripleExpr, ctx.unaryTripleExpr())
    } yield ses.length match {
      case 1 => ses.head
      case _ => EachOf(exprs = ses) // W , None, None)
    }

  override def visitMultiElementOneOf(ctx: MultiElementOneOfContext): Builder[TripleExpr] =
    for {
      groups <- visitList(visitGroupTripleExpr, ctx.groupTripleExpr)
    } yield groups.length match {
      case 1 => groups.head
      case _ =>
        OneOf(exprs = groups) // W, None, None)
    }

  override def visitShapeExprLabel(ctx: ShapeExprLabelContext): Builder[ShapeLabel] =
    ctx match {
      case _ if isDefined(ctx.iri()) =>
        for {
          iri <- visitIri(ctx.iri())
        } yield IRILabel(iri)
      case _ if isDefined(ctx.blankNode()) =>
        for {
          bNode <- visitBlankNode(ctx.blankNode())
        } yield BNodeLabel(bNode)
      case _ => err("visitShapeExprLabel, no IRI and no BNode")
    }

  override def visitBlankNode(ctx: BlankNodeContext): Builder[BNode] =
    ok(BNode(removeUnderscore(ctx.BLANK_NODE_LABEL().getText())))

  def removeUnderscore(str: String): String =
    str.drop(2)

  def getPrefixes(ds: List[Directive]): Map[Prefix, IRI] = {
    def comb(rest: Map[Prefix, IRI], x: Directive): Map[Prefix, IRI] =
      x.fold(p => rest + p, _ => rest)

    def zero: Map[Prefix, IRI] = Map()

    ds.foldLeft(zero)(comb)
  }

  override def visitDirective(ctx: DirectiveContext): Builder[Directive] = ctx match {
    case _ if isDefined(ctx.baseDecl()) =>
      for {
        iri <- visitBaseDecl(ctx.baseDecl())
      } yield Right(Left(iri))
    case _ if isDefined(ctx.prefixDecl()) =>
      for {
        p <- visitPrefixDecl(ctx.prefixDecl())
      } yield Left(p)
    case _ if isDefined(ctx.importDecl()) =>
      for {
        iri <- visitImportDecl(ctx.importDecl())
      } yield Right(Right(iri))
    case _ => err(s"visitDirective: unknown directive")
  }

  override def visitImportDecl(ctx: ImportDeclContext): Builder[IRI] = for {
    iri <- visitIri(ctx.iri())
  } yield iri

  override def visitBaseDecl(ctx: BaseDeclContext): Builder[IRI] =
    for {
      previousBase <- getBase
      baseIri <- extractIRIfromIRIREF(ctx.IRIREF().getText, previousBase)
      _ <- addBase(baseIri)
    } yield baseIri

  override def visitPrefixDecl(ctx: PrefixDeclContext): Builder[(Prefix, IRI)] =
    if (ctx.PNAME_NS() == null) err(s"Invalid prefix declaration")
    else {
      val prefix = Prefix(ctx.PNAME_NS().getText.init)
      for {
        iri <- extractIRIfromIRIREF(ctx.IRIREF().getText, None)
        _ <- addPrefix(prefix, iri)
      } yield (prefix, iri)
    }

  // Some generic utils

  private def isDefined[A](x: A): Boolean = x != null

  private def visitList[A, B](visitFn: A => Builder[B], ls: java.util.List[A]): Builder[List[B]] = {
    val bs: List[Builder[B]] = ls.asScala.toList.map(visitFn(_))
    sequence(bs)
  }

  private def visitOpt[A, B](visitFn: A => Builder[B], v: A): Builder[Option[B]] =
    if (isDefined(v)) visitFn(v).map(Some(_))
    else ok(None)

  private def getLanguage(str: String): Lang =
    Lang(str)
}

sealed trait Qualifier {

  def getExtras: List[PropertyId] =
    this match {
      case Extra(iris) => iris
      case _           => List()
    }
  def getExtends: List[ShapeLabel] =
    this match {
      case Extends(labels) => labels
      case _               => List()
    }
  def getRestricts: List[ShapeLabel] =
    this match {
      case Restricts(labels) => labels
      case _                 => List()
    }

  def getTermConstraints: List[TermConstraint] =
    this match {
      case TermConstraintQ(tcs) => tcs
      case _                    => List()
    }
}

case class Extra(ps: List[PropertyId]) extends Qualifier
case class Extends(labels: List[ShapeLabel]) extends Qualifier
case class Restricts(labels: List[ShapeLabel]) extends Qualifier
case object Closed extends Qualifier
case class TermConstraintQ(tcs: List[TermConstraint]) extends Qualifier
