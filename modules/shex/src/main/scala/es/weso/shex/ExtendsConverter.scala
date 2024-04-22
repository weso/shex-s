package es.weso.shex

import cats.effect._
import cats._
import cats.data._
import cats.implicits._
import scala.util.control.NoStackTrace

object ExtendsConverter {

  case class State(m: Map[ShapeLabel, ResolvedShapeExpr])
  object State {
    def empty = State(Map())
  }

  type Cnv[A] = ReaderT[IO, Set[ShapeLabel], A]
  private def fromIO[A](x: IO[A]): Cnv[A] = ReaderT.liftF(x)
  private def err[A](msg: String): Cnv[A] = fromIO(IO.raiseError(ConversionError(msg)))
  private def ok[A](x: A): Cnv[A] = x.pure[Cnv]
  private def info[A](msg: String): Cnv[Unit] = fromIO(IO(println(msg)))
  private def getVisited: Cnv[Set[ShapeLabel]] = ReaderT.ask
  private def withVisited[A](lbl: ShapeLabel, cnv: Cnv[A]): Cnv[A] =
    ReaderT.local((ls: Set[ShapeLabel]) => ls + lbl)(cnv)

  private def showId(schema: ResolvedSchema)(lbl: Option[ShapeLabel]): String = lbl match {
    case None    => ""
    case Some(l) => schema.qualify(l)
  }

  def convert(s: ResolvedSchema): IO[ResolvedSchema] = (for {
    nMap <- cnvMap(s)(s.resolvedMapShapeExprs)
    r = ResolvedSchema(
      source = s.source,
      resolvedMapShapeExprs = nMap,
      resolvedMapTripleExprs = s.resolvedMapTripleExprs,
      inheritanceGraph = s.inheritanceGraph,
      labelLocationMap = s.labelLocationMap
    )
  } yield r).run(Set())

  private def cnvMap(s: ResolvedSchema)(
      m: Map[ShapeLabel, ResolvedShapeExpr]
  ): Cnv[Map[ShapeLabel, ResolvedShapeExpr]] =
    mapValuesCnv(m)(cnvSe(s))

  private def cnvSe(s: ResolvedSchema)(se: ResolvedShapeExpr): Cnv[ResolvedShapeExpr] = for {
    newSe <- transSe(s)(se.se)
  } yield se.copy(se = newSe)

  private def transSe(schema: ResolvedSchema)(se: ShapeExpr): Cnv[ShapeExpr] = se match {
    case nk: NodeConstraint => mkDisj(schema)(nk)
    case s: Shape =>
      for {
        ts <- transShape(schema, s)
        ds <- cnvDescendants(schema)(s)
        r <-
          if (ds.isEmpty) ok(ts)
          else ok(ShapeOr(id = s.id, shapeExprs = (ts :: ds).map(_.rmId), None, None))
      } yield r
    case s: ShapeAnd =>
      for {
        ses <- s.shapeExprs.map(transSe(schema)).sequence
      } yield s.copy(shapeExprs = ses)
    case s: ShapeOr =>
      for {
        ses <- s.shapeExprs.map(transSe(schema)).sequence
      } yield s.copy(shapeExprs = ses)
    case s: ShapeNot =>
      for {
        newS <- transSe(schema)(s.shapeExpr)
      } yield s.copy(shapeExpr = newS)
    case sr: ShapeRef      => ok(sr)
    case se: ShapeExternal => mkDisj(schema)(se)
    case ShapeDecl(lbl, _, true) =>
      for {
        _ <- info(s"Abstract: ${schema.qualify(lbl)}")
        maybeR <- cnvDescendants(schema)(se)
        r <- maybeR match {
          case Nil      => err[ShapeExpr](s"No descendants for ${schema.qualify(lbl)}?")
          case s :: Nil => ok(s.addId(lbl))
          case _        => ok(ShapeOr(id = Some(lbl), shapeExprs = maybeR.map(_.rmId), None, None))
        }
      } yield r
    case ShapeDecl(lbl, se, false) =>
      cnvDescendants(schema)(se).flatMap(ds =>
        ds match {
          case Nil      => err[ShapeExpr](s"No descendants for ${schema.qualify(lbl)}?")
          case s :: Nil => ok(s.addId(lbl))
          case _        => ok(ShapeOr(id = Some(lbl), shapeExprs = ds.map(_.rmId), None, None))
        }
      )
  }

  private def mkDisj(schema: ResolvedSchema)(se: ShapeExpr): Cnv[ShapeExpr] = for {
    tse <- transSe(schema)(se)
    ds <- cnvDescendants(schema)(se)
    r <-
      if (ds.isEmpty) ok(tse)
      else ok(ShapeOr(id = se.id, shapeExprs = tse :: ds, None, None))
  } yield r

  private def transShape(schema: ResolvedSchema, shape: Shape): Cnv[Shape] =
    shape._extends match {
      case None | Some(Nil) =>
        shape.expression match {
          case Some(te) =>
            for {
              newTe <- transTe(schema)(te)
            } yield shape.copy(
              _extends = None,
              expression = Some(newTe)
            )
          case None => ok(shape)
        }
      case Some(es) =>
        info(
          s"transShape (shape=${shape
              .showQualified(schema.prefixMap)}) with extends: ${es.map(schema.qualify(_)).mkString(",")}\n"
        ) *>
          embedLs(schema)(es, shape)
    }

  private def embedLs(schema: ResolvedSchema)(ls: List[ShapeLabel], shape: Shape): Cnv[Shape] = {
    def cmb(s: Shape, lbl: ShapeLabel): Cnv[Shape] = for {
      _ <- info(
        s"embedLs.comb(${s.showQualified(schema.prefixMap)}, lbl=${schema.qualify(lbl)})???"
      )
      se <- getShapeLabel(schema)(lbl)
      /*      tse <- s.id match {
        case Some(slbl) => withVisited(slbl,transSe(schema)(se))
        case None => ok(se)
      } */
      newSe <- embedSe(schema)(se, s)
      _ <- info(s"embedLs.comb(s=${s.showQualified(schema.prefixMap)}, lbl=${schema
          .qualify(lbl)})=${newSe.showQualified(schema.prefixMap)}")
    } yield newSe
    Foldable[List].foldM(ls, shape)(cmb)
  }

  private def embedSe(schema: ResolvedSchema)(se: ShapeExpr, shape: Shape): Cnv[Shape] =
    info(
      s"embedSe(${se.showQualified(schema.prefixMap)},${shape.showQualified(schema.prefixMap)})"
    ) *>
      (se match {
        case nc: NodeConstraint => err(s"embed(NodeConstraint($nc),${shape}")
        case s: Shape =>
          for {
            r <- embedShape(schema)(s, shape)
            _ <- info(s"embedSe(se=${se.showQualified(schema.prefixMap)},shape=${shape
                .showQualified(schema.prefixMap)})=${r.showQualified(schema.prefixMap)}")
          } yield r
        case sa: ShapeAnd => /* {
      def cmb(s: Shape, se: ShapeExpr): Cnv[Shape] = embedSe(schema)(se,s)
      sa.shapeExprs.foldM(shape)(cmb) //
    } */
          err(s"not implemented embed(ShapeAnd($sa),${shape}")
        case so: ShapeOr  => err(s"not implementedembed(ShapeOr($so),${shape}")
        case sn: ShapeNot => err(s"not implemented embed(ShapeNot($sn),${shape}")
        case sr: ShapeRef =>
          for {
            se <- getShapeLabel(schema)(sr.reference)
            r <- embedSe(schema)(se, shape)
          } yield r // err(s"not implemented embed(ShapeRef($sr),${shape}")
        case se: ShapeExternal => err(s"not implemented embed(ShapeExternal($se),${shape})")
        case sd: ShapeDecl     => embedSe(schema)(sd.shapeExpr, shape)
      })

  private def embedShape(schema: ResolvedSchema)(shape1: Shape, shape2: Shape): Cnv[Shape] =
    for {
      // _ <- info(s"before embedShape(shape1=${shape1.showQualified(schema.prefixMap)},shape2=${shape2
      //    .showQualified(schema.prefixMap)})")
      newShape1 <- shape2.id match {
        case Some(s2lbl) => withVisited(s2lbl, transShape(schema, shape1))
        case None        => ok(shape1)
      }
      // _ <- info(s"expr1 = ${newShape1.expression}, expr2=${shape2.expression}")
      r <- ok(
        shape2.copy(
          expression = embedOptTe(schema)(newShape1.expression, shape2.expression),
          _extends = None
        )
      )
      _ <- info(s"after embedShape(shape1=${shape1.showQualified(schema.prefixMap)},shape2=${shape2
          .showQualified(schema.prefixMap)})=${r.showQualified(schema.prefixMap)}")
    } yield r

  private def embedOptTe(
      schema: ResolvedSchema
  )(maybeTe1: Option[TripleExpr], maybeTe2: Option[TripleExpr]): Option[TripleExpr] =
    (maybeTe1, maybeTe2) match {
      case (None, None)           => None
      case (Some(te), None)       => Some(te)
      case (None, Some(te))       => Some(te)
      case (Some(te1), Some(te2)) => Some(embedTe(te1, te2))
    }

  private def embedTe(te1: TripleExpr, te2: TripleExpr): TripleExpr =
    EachOf(None, expressions = List(te1, te2), Some(1), Some(IntMax(1)), None, None)

  private def transTe(schema: ResolvedSchema)(te: TripleExpr): Cnv[TripleExpr] = te match {
    case eo: EachOf =>
      for {
        tes <- eo.expressions.map(transTe(schema)).sequence
      } yield eo.copy(expressions = tes)
    case oo: OneOf =>
      for {
        tes <- oo.expressions.map(transTe(schema)).sequence
      } yield oo.copy(expressions = tes)
    case i: Inclusion         => ok(i)
    case e: Expr              => ok(e)
    case tc: TripleConstraint => ok(tc)
  }

  private def cnvDescendants(schema: ResolvedSchema)(se: ShapeExpr): Cnv[List[ShapeExpr]] =
    se.id match {
      case None => info(s"No id for se=${se}?") *> ok(List())
      case Some(lbl) =>
        for {
          ds <- fromIO(schema.inheritanceGraph.ancestors(lbl))
          visited <- getVisited
          toCheck = ds.filter(x => !(visited contains x))
          _ <- info(
            s"""|Descendants of ${showId(schema)(se.id)}=${ds.map(schema.qualify(_)).mkString(",")}
                    |Visited: ${visited}
                    |Effective descendants: ${toCheck}
                    |""".stripMargin
          )
          newSe <-
            if (toCheck.isEmpty) ok(List())
            else
              for {
                dses <- toCheck.map(getShapeLabel(schema)).toList.sequence
                rs <- dses.map(transSe(schema)).sequence
                _ <- info(
                  s"Descendants converted:\n${rs.map(_.showQualified(schema.prefixMap)).mkString("\n")}\n---\n"
                )
              } yield rs
        } yield newSe
    }

  private def mapValuesCnv[A, B](m: Map[A, B])(f: B => Cnv[B]): Cnv[Map[A, B]] = {
    val vs = m.toList.traverse { case (k, v) => f(v).map(v1 => (k, v1)) }
    vs.map(_.toMap)
  }

  def getShapeLabel(schema: ResolvedSchema)(sl: ShapeLabel): Cnv[ShapeExpr] =
    schema.getShape(sl) match {
      case Left(str) =>
        err(
          s"Not found label: ${sl.toRDFNode.getLexicalForm} in schema. Available labels: ${schema.labels
              .map(_.toRDFNode.getLexicalForm)
              .mkString(",")}"
        )
      case Right(se) => ok(se)
    }

}

case class ConversionError(msg: String) extends RuntimeException(msg) with NoStackTrace
