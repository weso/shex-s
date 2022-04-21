package es.weso.shex.shexR

import es.weso.shex._
import PREFIXES._
import cats.implicits._
import es.weso.rdf.nodes._
import es.weso.rdf.PREFIXES._
import es.weso.rdf.RDFBuilder
import es.weso.rdf.saver.RDFSaver
import es.weso.rdf.operations.Comparisons._
import cats.effect._

trait ShEx2RDF extends RDFSaver {

  def serialize(
      shex: Schema,
      node: Option[IRI],
      format: String,
      rdfBuilder: RDFBuilder
  ): IO[String] =
    for {
      rdf <- toRDF(shex, node, rdfBuilder)
      str <- rdf.serialize(format)
    } yield str

  def toRDF(s: Schema, node: Option[IRI], initial: RDFBuilder): IO[RDFBuilder] =
    schema(s, node).run(initial).map(_._1)

  private def schema(s: Schema, id: Option[IRI]): RDFSaver[Unit] =
    for {
      node <- makeId(id)
      _ <- addPrefixMap(s.prefixMap)
      _ <- addTriple(node, `rdf:type`, sx_Schema)
      _ <- maybeAddListContent(s.startActs, node, sx_startActs, semAct)
      _ <- maybeAddContent(s.start, node, sx_start, shapeExpr)
      _ <- maybeAddStarContent(s.shapes, node, sx_shapes, shapeExpr)
    } yield ()

  private def shapeExpr(e: ShapeExpr): RDFSaver[RDFNode] = e match {
    case sa: ShapeAnd =>
      for {
        node <- mkId(sa.id)
        _ <- addTriple(node, `rdf:type`, sx_ShapeAnd)
        _ <- addListContent(sa.shapeExprs, node, sx_expressions, shapeExpr)
        _ <- maybeAddListContent(sa.actions, node, sx_semActs, semAct)
        _ <- maybeAddStarContent(sa.annotations, node, sx_annotation, annotation)
      } yield node

    case so: ShapeOr =>
      for {
        node <- mkId(so.id)
        _ <- addTriple(node, `rdf:type`, sx_ShapeOr)
        _ <- addListContent(so.shapeExprs, node, sx_expressions, shapeExpr)
        _ <- maybeAddListContent(so.actions, node, sx_semActs, semAct)
        _ <- maybeAddStarContent(so.annotations, node, sx_annotation, annotation)
      } yield node

    case sn: ShapeNot =>
      for {
        node <- mkId(sn.id)
        _ <- addTriple(node, `rdf:type`, sx_ShapeNot)
        _ <- addContent(sn.shapeExpr, node, sx_expression, shapeExpr)
        _ <- maybeAddListContent(sn.actions, node, sx_semActs, semAct)
        _ <- maybeAddStarContent(sn.annotations, node, sx_annotation, annotation)
      } yield node

    case nk: NodeConstraint =>
      for {
        shapeId <- mkId(nk.id)
        _ <- addTriple(shapeId, `rdf:type`, sx_NodeConstraint)
        _ <- maybeAddContent(nk.nodeKind, shapeId, sx_nodeKind, nodeKind)
        _ <- maybeAddContent(nk.datatype, shapeId, sx_datatype, iri)
        _ <- nk.xsFacets.map(xsFacet(_, shapeId)).sequence
        _ <- maybeAddListContent(nk.values, shapeId, sx_values, valueSetValue)
        _ <- maybeAddListContent(nk.actions, shapeId, sx_semActs, semAct)
        _ <- maybeAddStarContent(nk.annotations, shapeId, sx_annotation, annotation)
      } yield shapeId

    case s: Shape =>
      for {
        shapeId <- mkId(s.id)
        _ <- addTriple(shapeId, `rdf:type`, sx_Shape)
        _ <- maybeAddContent(s.closed, shapeId, sx_closed, rdfBoolean)
        _ <- maybeAddStarContent(s.extra, shapeId, sx_extra, iri)
        _ <- maybeAddContent(s.expression, shapeId, sx_expression, tripleExpr)
        _ <- maybeAddListContent(s.actions, shapeId, sx_semActs, semAct)
        _ <- maybeAddStarContent(s.annotations, shapeId, sx_annotation, annotation)
      } yield shapeId

    case se: ShapeExternal =>
      for {
        shapeId <- mkId(se.id)
        _ <- addTriple(shapeId, `rdf:type`, sx_ShapeExternal)
        _ <- maybeAddListContent(se.actions, shapeId, sx_semActs, semAct)
        _ <- maybeAddStarContent(se.annotations, shapeId, sx_annotation, annotation)
      } yield shapeId

    case sr: ShapeRef =>
      for {
        shapeId <- label(sr.reference)
        _ <- maybeAddListContent(sr.actions, shapeId, sx_semActs, semAct)
        _ <- maybeAddStarContent(sr.annotations, shapeId, sx_annotation, annotation)
      } yield shapeId

    case sd: ShapeDecl =>
      for {
        shapeId <- mkId(sd.id)
        _ <- addTriple(shapeId, `rdf:type`, sx_ShapeDecl)
        _ <- addContent(true, shapeId, sx_abstract, rdfBoolean)
        _ <- addContent(sd.shapeExpr, shapeId, sx_shapeExpr, shapeExpr)
      } yield shapeId

  }

  private def xsFacet(facet: XsFacet, node: RDFNode): RDFSaver[Unit] = facet match {
    case Length(n)    => addTriple(node, sx_length, IntegerLiteral(n))
    case MinLength(n) => addTriple(node, sx_minlength, IntegerLiteral(n))
    case MaxLength(n) => addTriple(node, sx_maxlength, IntegerLiteral(n))
    case Pattern(n, flags) =>
      for {
        _ <- addTriple(node, sx_pattern, StringLiteral(n))
        _ <- maybeAddTriple(node, sx_flags, flags.map(StringLiteral(_)))
      } yield ()
    case MinInclusive(n)   => addContent(n, node, sx_mininclusive, numericLiteral)
    case MinExclusive(n)   => addContent(n, node, sx_minexclusive, numericLiteral)
    case MaxInclusive(n)   => addContent(n, node, sx_maxinclusive, numericLiteral)
    case MaxExclusive(n)   => addContent(n, node, sx_maxexclusive, numericLiteral)
    case FractionDigits(n) => addTriple(node, sx_fractiondigits, IntegerLiteral(n))
    case TotalDigits(n)    => addTriple(node, sx_totaldigits, IntegerLiteral(n))
  }

  private def numericLiteral(n: NumericLiteral): RDFSaver[RDFNode] = n match {
    case NumericInt(n, repr)     => ok(IntegerLiteral(n, repr))
    case NumericDouble(n, repr)  => ok(DoubleLiteral(n, repr))
    case NumericDecimal(n, repr) => ok(DecimalLiteral(n, repr))
  }

  private def valueSetValue(x: ValueSetValue): RDFSaver[RDFNode] = x match {
    case IRIValue(iri)          => ok(iri)
    case StringValue(s)         => ok(StringLiteral(s))
    case DatatypeString(s, iri) => ok(DatatypeLiteral(s, iri))
    case LangString(s, lang)    => ok(LangLiteral(s, lang))
    case Language(Lang(str)) =>
      ok(
        StringLiteral(str)
      ) // TODO: This one looks suspicious (it seems to have the same representation as a string)
    case s: IRIStem           => iriStem(s)
    case i: IRIStemRange      => iriStemRange(i)
    case s: LanguageStem      => languageStem(s)
    case l: LanguageStemRange => languageStemRange(l)
    case l: LiteralStem       => literalStem(l)
    case l: LiteralStemRange  => literalStemRange(l)
  }

  private def iriExclusion(iri: IRIExclusion): RDFSaver[RDFNode] = iri match {
    case IRIRefExclusion(iri)   => ok(iri)
    case IRIStemExclusion(stem) => iriStem(stem)
  }

  private def literalExclusion(le: LiteralExclusion): RDFSaver[RDFNode] = le match {
    case LiteralStringExclusion(str) => ok(StringLiteral(str))
    case LiteralStemExclusion(stem)  => literalStem(stem)
  }

  private def languageExclusion(le: LanguageExclusion): RDFSaver[RDFNode] = le match {
    case LanguageTagExclusion(Lang(str)) => ok(StringLiteral(str))
    case LanguageStemExclusion(stem)     => languageStem(stem)
  }

  private def iriStem(x: IRIStem): RDFSaver[RDFNode] = for {
    node <- createBNode()
    _ <- addTriple(node, `rdf:type`, sx_IriStem)
    _ <- addTriple(node, sx_stem, DatatypeLiteral(x.stem.str, `xsd:anyUri`))
  } yield node

  private def languageStem(x: LanguageStem): RDFSaver[RDFNode] = for {
    node <- createBNode()
    _ <- addTriple(node, `rdf:type`, sx_LanguageStem)
    _ <- addTriple(node, sx_stem, StringLiteral(x.stem.lang))
  } yield node

  private def literalStem(x: LiteralStem): RDFSaver[RDFNode] = for {
    node <- createBNode()
    _ <- addTriple(node, `rdf:type`, sx_LiteralStem)
    _ <- addTriple(node, sx_stem, StringLiteral(x.stem))
  } yield node

  private def iriStemRange(range: IRIStemRange): RDFSaver[RDFNode] = for {
    node <- createBNode()
    _ <- addTriple(node, `rdf:type`, sx_IriStemRange)
    _ <- addContent(range.stem, node, sx_stem, iriStemRangeValue)
    _ <- maybeAddStarContent(range.exclusions, node, sx_exclusion, iriExclusion)
  } yield node

  private def iriStemRangeValue(x: IRIStemRangeValue): RDFSaver[RDFNode] = x match {
    case IRIStemValueIRI(iri) => ok(DatatypeLiteral(iri.str, `xsd:anyUri`))
    case IRIStemWildcard() =>
      for {
        node <- createBNode()
        _ <- addTriple(node, `rdf:type`, sx_Wildcard)
      } yield node
  }

  private def literalStemRange(range: LiteralStemRange): RDFSaver[RDFNode] = for {
    node <- createBNode()
    _ <- addTriple(node, `rdf:type`, sx_LiteralStemRange)
    _ <- addContent(range.stem, node, sx_stem, literalStemRangeValue)
    _ <- maybeAddStarContent(range.exclusions, node, sx_exclusion, literalExclusion)
  } yield node

  private def literalStemRangeValue(x: LiteralStemRangeValue): RDFSaver[RDFNode] = x match {
    case LiteralStemRangeString(str) => ok(StringLiteral(str))
    case LiteralStemRangeWildcard() =>
      for {
        node <- createBNode()
        _ <- addTriple(node, `rdf:type`, sx_Wildcard)
      } yield node
  }

  private def languageStemRange(range: LanguageStemRange): RDFSaver[RDFNode] = for {
    node <- createBNode()
    _ <- addTriple(node, `rdf:type`, sx_LanguageStemRange)
    _ <- addContent(range.stem, node, sx_stem, languageStemRangeValue)
    _ <- maybeAddStarContent(range.exclusions, node, sx_exclusion, languageExclusion)
  } yield node

  private def languageStemRangeValue(x: LanguageStemRangeValue): RDFSaver[RDFNode] = x match {
    case LanguageStemRangeLang(Lang(lang)) => ok(StringLiteral(lang))
    case LanguageStemRangeWildcard() =>
      for {
        node <- createBNode()
        _ <- addTriple(node, `rdf:type`, sx_Wildcard)
      } yield node
  }

  private def tripleExpr(te: TripleExpr): RDFSaver[RDFNode] =
    te match {
      // TODO: Variable declaration
      case TripleConstraint(
            id,
            inverse,
            negated,
            pred,
            valueExpr,
            min,
            max,
            _,
            semActs,
            annotations
          ) =>
        for {
          teId <- mkId(id)
          _ <- addTriple(teId, `rdf:type`, sx_TripleConstraint)
          _ <- maybeAddContent(inverse, teId, sx_inverse, rdfBoolean)
          _ <- maybeAddContent(negated, teId, sx_negated, rdfBoolean)
          _ <- addTriple(teId, sx_predicate, pred)
          _ <- maybeAddContent(valueExpr, teId, sx_valueExpr, shapeExpr)
          _ <- maybeAddContent(min, teId, sx_min, rdfInt)
          _ <- maybeAddContent(max, teId, sx_max, rdfMax)
          _ <- maybeAddListContent(semActs, teId, sx_semActs, semAct)
          _ <- maybeAddStarContent(annotations, teId, sx_annotation, annotation)
        } yield teId
      case EachOf(id, exprs, min, max, semActs, annotations) =>
        for {
          node <- mkId(id)
          _ <- addTriple(node, `rdf:type`, sx_EachOf)
          _ <- addListContent(exprs, node, sx_expressions, tripleExpr)
          _ <- maybeAddContent(min, node, sx_min, rdfInt)
          _ <- maybeAddContent(max, node, sx_max, rdfMax)
          _ <- maybeAddListContent(semActs, node, sx_semActs, semAct)
          _ <- maybeAddStarContent(annotations, node, sx_annotation, annotation)
        } yield node
      case OneOf(id, exprs, min, max, semActs, annotations) =>
        for {
          node <- mkId(id)
          _ <- addTriple(node, `rdf:type`, sx_OneOf)
          _ <- addListContent(exprs, node, sx_expressions, tripleExpr)
          _ <- maybeAddContent(min, node, sx_min, rdfInt)
          _ <- maybeAddContent(max, node, sx_max, rdfMax)
          _ <- maybeAddListContent(semActs, node, sx_semActs, semAct)
          _ <- maybeAddStarContent(annotations, node, sx_annotation, annotation)
        } yield node
      case Inclusion(lbl) => label(lbl)
      case e: Expr =>
        // TODO
        val msg = s"Expr serialization to RDF not implemented yet. Expr = $e"
        println(msg)
        ok(StringLiteral(msg))
    }

  private def semAct(x: SemAct): RDFSaver[RDFNode] = for {
    id <- createBNode()
    _ <- addTriple(id, `rdf:type`, sx_SemAct)
    _ <- addTriple(id, sx_name, x.name)
    _ <- maybeAddContent(x.code, id, sx_code, rdfString)
  } yield id

  private def rdfMax(x: Max): RDFSaver[RDFNode] = x match {
    case IntMax(n) => rdfInt(n)
    case Star      => ok(sx_INF)
  }

  private def annotation(x: Annotation): RDFSaver[RDFNode] = for {
    id <- createBNode()
    _ <- addTriple(id, `rdf:type`, sx_Annotation)
    _ <- addTriple(id, sx_predicate, x.predicate)
    _ <- addContent(x.obj, id, sx_object, objectValue)
  } yield id

  private def objectValue(x: ObjectValue): RDFSaver[RDFNode] = x match {
    case IRIValue(iri)          => ok(iri)
    case StringValue(s)         => ok(StringLiteral(s))
    case DatatypeString(s, iri) => ok(DatatypeLiteral(s, iri))
    case LangString(s, lang)    => ok(LangLiteral(s, lang))
  }

  private def label(lbl: ShapeLabel): RDFSaver[RDFNode] = lbl match {
    case Start         => ok(sx_start)
    case l: IRILabel   => ok(l.iri)
    case l: BNodeLabel => ok(l.bnode)
  }

  private def nodeKind(nk: NodeKind): RDFSaver[RDFNode] =
    nk match {
      case IRIKind        => ok(sx_iri)
      case BNodeKind      => ok(sx_bnode)
      case LiteralKind    => ok(sx_literal)
      case NonLiteralKind => ok(sx_nonliteral)
    }

  private def mkId(id: Option[ShapeLabel]): RDFSaver[RDFNode] =
    id.map(label).getOrElse(createBNode())

}

object ShEx2RDF {

  def apply(s: Schema, n: Option[IRI], builder: RDFBuilder): IO[RDFBuilder] = {
    val srdf = new ShEx2RDF {}
    srdf.toRDF(s, n, builder)
  }

}
