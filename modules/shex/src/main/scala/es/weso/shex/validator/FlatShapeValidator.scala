package es.weso.shex.validator

import es.weso.shex._
import es.weso.rdf.nodes._
import es.weso.shex.normalized._
// import es.weso.shex.implicits.showShEx._
import cats.data._
import cats.effect.IO
//import cats._
import cats.implicits._
// import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.RDFReader
import es.weso.utils.eitherios.EitherIOUtils._
import es.weso.shex.validator.ShExError._
import es.weso.rdf.PrefixMap
import es.weso.rdf.RDFBuilder

/** FlatShape validator
  */
case class FlatShapeValidator(
    nodesPrefixMap: PrefixMap,
    shapesPrefixMap: PrefixMap,
    builder: RDFBuilder,
    schema: ResolvedSchema
) extends ShExChecker
    with ShowValidator {

  private def showId(s: FlatShape): String =
    s.id.map(lbl => shapesPrefixMap.qualify(lbl.toRDFNode)).getOrElse(s.show)

  private[validator] def checkFlatShape(
      attempt: Attempt,
      node: RDFNode,
      shape: FlatShape,
      ext: Option[Neighs]
  ): CheckTyping = {
    val zero = getTyping
    def cmb(checkTyping: CheckTyping, slot: (Path, Constraint)): CheckTyping = {
      val (path, constraint) = slot
      debug(s""" checkFlatShapeSlot(${node.show}@${shape.id.getOrElse(
                "?"
              )}) path: ${path.show}, constraint=${constraint.show}""".stripMargin) *>
        checkTyping.flatMap(t1 =>
          checkConstraint(attempt, node, path, constraint, ext).flatMap(t2 =>
            infoTyping(
              t2,
              s"Result checkFlatShapeSlot(${node.show}@${shape.id.getOrElse("?")})",
              schema.prefixMap
            ) *>
              combineTypings(t1, t2)
          )
        )
    }
    for {
      _ <- debug(s"""|checkFlatShape(${node.show}@${showId(shape)})""".stripMargin)
      extra <- extraPreds(node, shape.preds, ext)
      _ <- debug(s"Extra preds: $extra. Closed? ${shape.closed}")
      _ <- debug(s"Slots:\n${shape.slots.map(slot => s"--slot: ${slot.show}").mkString("\n")}")
      typing <-
        if (shape.closed && extra.nonEmpty) {
          errClosedButExtraPreds(extra)
        } else
          shape.slots.foldLeft(zero)(cmb)
      _ <- debug(s"FlatShape(${node.show}@${showId(shape)}}) successful")
    } yield typing
  }

  private def errClosedButExtraPreds(extra: Set[IRI]): CheckTyping = err(ClosedButExtraPreds(extra))

  // Returns the list of paths that are different from a given list
  private def extraPreds(node: RDFNode, preds: Set[IRI], ext: Option[Neighs]): Check[Set[IRI]] =
    for {
      existingPreds <- getExistingPredicates(node, ext)
    } yield existingPreds -- preds

  private def checkConstraint(
      attempt: Attempt,
      node: RDFNode,
      path: Path,
      constraint: Constraint,
      ext: Option[Neighs]
  ): CheckTyping =
    for {
      _ <- debug(
        s"checkConstraint: ${constraint.show} for ${showNode(node)} with path ${path.show}"
      )
      values <- getValuesPath(node, path, ext)
      _ <- debug(
        s"Values of node ${showNode(node)} with path ${path.show} = [${values.map(_.show).mkString(",")}]"
      )
      typing <- checkValuesConstraint(values, constraint, node, path, attempt)
      _ <- debug(
        s"After checkConstraint(${node.show}, Constraint: ${constraint.show}): ${typing.show}"
      )
    } yield typing

  private def getExistingPredicates(node: RDFNode, ext: Option[Neighs]): Check[Set[IRI]] =
    getNeighs(node, ext).map(_.getPredicates())

  // We assume that the shape has no reference to other shapes
  private def checkValuesConstraint(
      values: Set[RDFNode],
      constraint: Constraint,
      node: RDFNode,
      path: Path,
      attempt: Attempt
  ): CheckTyping = {
    val card: Cardinality = constraint.card
    constraint.shape match {
      case None =>
        if (card.contains(values.size))
          addEvidence(
            attempt.nodeShape,
            s"Node ${node.show}: number of values (${values.size}) for path ${path.show} fits card ${card.show}"
          )
        else
          for {
            rdf <- getRDF
            _ <- debug(s"Cardinality error ${values.size} for path ${path.show} ${card.show}")
            r <- err[ShapeTyping](ErrCardinality(attempt, node, path, values.size, card, rdf))
          } yield r
      case Some(se) =>
        if (constraint.hasExtra) {
          for {
            // _ <- debug(s"Constraint ${constraint.show} has EXTRA")
            rdf <- getRDF
            t <- {
              val rs: List[EitherT[IO, String, String]] =
                values.toList.map(checkNodeShapeExprBasic(_, se, rdf))
              val doPartition: Check[(List[String], List[String])] = fromIO(partitionEitherIOS(rs))
              val p: Check[ShapeTyping] = for {
                partition <- doPartition
                (notPassed, passed) = partition
                // _ <- debug(s"Partition: \nPassed: ${(passed)}\nNot passed: ${notPassed}")
                t <-
                  if (card.contains(passed.size)) {
                    addEvidence(
                      attempt.nodeShape,
                      s"Number of values for ${showNode(node)} with ${path
                          .showQualified(nodesPrefixMap)} that satisfy ${constraint.shape
                          .map(showSE(_))
                          .getOrElse("<Empty>")} = ${passed.size} matches cardinality ${constraint.card.show}"
                    )
                  } else
                    errCardinalityExtra(attempt, node, path, passed.size, notPassed.size, card, rdf)
              } yield t
              p
            }
          } yield t
        } else {
          debug(s"checkValuesConstraint...else") *>
            getRDF.flatMap(rdf =>
              if (constraint.card.contains(values.size)) {
                val rs: List[EitherT[IO, (RDFNode, String), (RDFNode, String)]] =
                  injectPairLs(values.toList.map(v => (v, checkNodeShapeExprBasic(v, se, rdf))))
                /*val doPartition: Check[(List[(RDFNode, String)], List[(RDFNode, String)])] =
                fromIO(partitionEitherIOS(rs)) */
                debug(s"before doPartition") *>
                  fromIO(partitionEitherIOS(rs)).flatMap { partition =>
                    val (notPassed, passed) = partition
                    if (notPassed.isEmpty) {
                      debug(s"notPassed is empty => all passed :)") *>
                        addEvidence(
                          attempt.nodeShape,
                          s"${showNode(node)} passed ${constraint.showQualified(shapesPrefixMap)} for path ${path
                              .showQualified(nodesPrefixMap)}"
                        )
                    } else {
                      debug(s"checkValuesConstraint($node) failed: Passed: ${passed
                          .map(_.toString)
                          .mkString(s",")}| No passed: ${notPassed.map(_.toString).mkString(s",")}") *>
                        err[ShapeTyping](
                          ValuesNotPassed(attempt, node, path, passed.size, notPassed.toSet, rdf)
                        )
                    }
                  }
              } else errCardinality(attempt, node, path, values.size, card, rdf)
            )
        }
      /*          for {
            // _ <- debug(s"Constraint has no EXTRA")
            rdf <- getRDF
            t <- if (constraint.card.contains(values.size)) {
              val rs: List[EitherT[IO, (RDFNode, String), (RDFNode, String)]] =
               injectPairLs(values.toList.map(v => (v, checkNodeShapeExprBasic(v, se, rdf))))
              val doPartition: Check[(List[(RDFNode, String)], List[(RDFNode, String)])] =
                fromIO(partitionEitherIOS(rs))
              for {
                partition <- doPartition
                (notPassed, passed) = partition
                // _ <- debug(s"checkValuesConstraint: Passed: ${passed.map(_.toString).mkString(s",")}| No passed: ${notPassed.map(_.toString).mkString(s",")}")
                newt <- if (notPassed.isEmpty) {
                  addEvidence(attempt.nodeShape, s"${showNode(node)} passed ${constraint.showQualified(shapesPrefixMap)} for path ${path.showQualified(nodesPrefixMap)}")
                } else
                  debug(s"checkValuesConstraint($node) failed: Passed: ${passed.map(_.toString).mkString(s",")}| No passed: ${notPassed.map(_.toString).mkString(s",")}")
                  err[ShapeTyping](ValuesNotPassed(attempt, node, path, passed.size, notPassed.toSet,rdf))
              } yield newt
             }
             else errCardinality(attempt,node,path,values.size, card, rdf)
          } yield t */
    }
  }

  private def errCardinality(
      attempt: Attempt,
      node: RDFNode,
      path: Path,
      size: Int,
      card: Cardinality,
      rdf: RDFReader
  ): CheckTyping =
    debug(
      s"Cardinality error on ${node.show} for path ${path.show}: ${size} doesn't match ${card.show}"
    ) >>
      err(ErrCardinality(attempt, node, path, size, card, rdf))

  private def errCardinalityExtra(
      attempt: Attempt,
      node: RDFNode,
      path: Path,
      passedSize: Int,
      notPassedSize: Int,
      card: Cardinality,
      rdf: RDFReader
  ): CheckTyping =
    debug(
      s"Cardinality error on ${node.show} for path ${path.show} with Extra: ${passedSize} ${card}"
    ) >>
      err(ErrCardinalityWithExtra(attempt, node, path, passedSize, notPassedSize, card, rdf))

  private def checkNodeShapeExprBasic(
      node: RDFNode,
      se: ShapeExpr,
      rdf: RDFReader
  ): EitherT[IO, String, String] =
    se match {
      case sa: ShapeAnd => cmb(sa.shapeExprs.map(checkNodeShapeExprBasic(node, _, rdf)))
      case so: ShapeOr  => cmb(so.shapeExprs.map(checkNodeShapeExprBasic(node, _, rdf)))
      case sn: ShapeNot =>
        checkNodeShapeExprBasic(node, sn.shapeExpr, rdf)
          .biflatMap(
            _ => mkOk(s"Passes negation"),
            _ => mkErr(s"Doesn't pass negation")
          )
      case _: ShapeRef => mkErr("Internal error. A normalized ShapeExpr cannot have references ")
      case s: Shape if s.isEmpty => mkOk(s"$node matches empty shape")
      case s: Shape              =>
        // checkShapeBase(Attempt(NodeShape(node, ShapeType(s,s.id, schema)),None), node, s)
        mkErr(s"checkNodeShapeExprBasic: Not implemented yet Shape ")
      case _: ShapeExternal => mkErr(s"Still don't know what to do with external shapes")
      case nk: NodeConstraint =>
        NodeConstraintChecker(schema, rdf, builder).nodeConstraintChecker(node, nk)
      case sd: ShapeDecl => mkErr(s"checkNodeShapeExprBasic: Not implemented yet ShapeDecl($sd)")
      // case _ => mkErr(s"checkNodeShapeExprBasic: Not implemented yet ShapeDecl($se)")
    }

  private def mkErr(s: String): EitherT[IO, String, String] =
    EitherT.fromEither(s.asLeft[String])

  private def mkOk(s: String): EitherT[IO, String, String] = EitherT.pure(s)

  private def cmb(els: List[EitherT[IO, String, String]]): EitherT[IO, String, String] =
    // val rs : EitherT[IO,String,List[String]] =
    els.sequence.map(_.mkString("\n"))

  def showNode(node: RDFNode): String = nodesPrefixMap.qualify(node)
  def showShape(shapeLabel: ShapeLabel): String = shapesPrefixMap.qualify(shapeLabel.toRDFNode)

}
