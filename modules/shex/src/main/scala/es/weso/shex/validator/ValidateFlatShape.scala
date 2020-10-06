package es.weso.shex.validator

import es.weso.shex._
import es.weso.rdf.nodes._
import ShExChecker._
import es.weso.shex.normalized._
// import es.weso.shex.implicits.showShEx._
import cats.data._
import cats.effect.IO
//import cats._
import cats.implicits._
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.RDFReader
import es.weso.utils.eitherios.EitherIOUtils._
import es.weso.shex.validator.ShExError._

/**
  * ShEx validator
  */
case class ValidateFlatShape(validator: Validator) {

  private[validator] def checkFlatShape(attempt: Attempt, node: RDFNode, s: FlatShape): CheckTyping = {
    val zero = getTyping
    def cmb(ct: CheckTyping, slot: (Path, Constraint)): CheckTyping = {
      val (path, constraint) = slot
      for {
        _ <- { info(s"CheckFlatShape/Constraint: path: $path\nConstraint: ${constraint.show}\nConstraint:${constraint}") }
        typing1 <- ct
        typing2 <- checkConstraint(attempt, node, path, constraint)
        typing  <- combineTypings(List(typing1, typing2))
      } yield {
        //println(s"Typing: ${typing.getMap}")
        typing
      }
    }
    for {
      _ <- info(s"### FlatShape:\n $s")
      extra <- extraPreds(node, s.preds)
      _ <- info(s"Extra preds: $extra. Closed? ${s.closed}")
      // _ <- ok(extra)
      typing <- if (s.closed && extra.nonEmpty) {
        err(ClosedButExtraPreds(extra))  // TODO: Not sure about this check
      } else 
        s.slots.foldLeft(zero)(cmb)
    } yield typing
  }

  // Returns the list of paths that are different from a given list
  private[validator] def extraPreds(node: RDFNode, preds: Set[IRI]): Check[Set[IRI]] =
    for {
      existingPreds <- getExistingPredicates(node)
    } yield existingPreds -- preds

  private[validator] def checkConstraint(attempt: Attempt, node: RDFNode, path: Path, constraint: Constraint): CheckTyping =
    for {
      _ <- info(s"checkConstraint: ${constraint.show} for ${node.show} with path ${path.show}")
      values <- getValuesPath(node, path)
      _ <- info(s"values for path: ${node.show} with path ${path.show} = [${values.map(_.show).mkString(",")}]")
      typing <- checkValuesConstraint(values, constraint, node, path, attempt)
    } yield typing

  private def getExistingPredicates(node: RDFNode): Check[Set[IRI]] =
    for {
      rdf <- getRDF
      ps  <- fromStream(rdf.triplesWithSubject(node))
    } yield ps.toSet[RDFTriple].map(_.pred)

  private def getValuesPath(node: RDFNode, path: Path): Check[Set[RDFNode]] =
    for {
      rdf   <- getRDF
      nodes <- fromStream(path.getValues(node, rdf))
    } yield nodes.toSet

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
        if (card.contains(values.size)) addEvidence(attempt.nodeShape, s"Number of values fits $card")
        else err(ErrCardinality(attempt, node, path, values.size, card))
      case Some(se) =>
        if (constraint.hasExtra) {
          for {
            _ <- info(s"Constraint has EXTRA")
            rdf <- getRDF
            t <- {
              val rs: List[EitherT[IO, String, String]]            = values.toList.map(checkNodeShapeExprBasic(_, se, rdf))
              val doPartition: Check[(List[String], List[String])] = fromIO(partitionEitherIOS(rs))
              val p: Check[ShapeTyping] = for {
                partition <- doPartition
                (notPassed, passed) = partition
                _ <- info(s"Partition: \nPassed: ${(passed)}\nNot passed: ${notPassed}")
                t <- if (card.contains(passed.size)) {
                  addEvidence(
                    attempt.nodeShape,
                    s"Number of values for ${node.show} with ${path.show} that satisfy ${constraint.shape} = ${passed.size} matches cardinality ${constraint.card}"
                  )
                } else {
                  err(ErrCardinalityWithExtra(attempt, node, path, passed.size, notPassed.size, card))
                }
              } yield t
              p
            }
          } yield t
        } else
          for {
            _ <- info(s"Constraint has no EXTRA")
            rdf <- getRDF
            t <- if (constraint.card.contains(values.size)) {
              val rs: List[EitherT[IO, (RDFNode, String), (RDFNode, String)]] =
               injectPairLs(values.toList.map(v => (v, checkNodeShapeExprBasic(v, se, rdf))))
              val doPartition: Check[(List[(RDFNode, String)], List[(RDFNode, String)])] =
                fromIO(partitionEitherIOS(rs))
              val ct: Check[ShapeTyping] = for {
                partition <- doPartition
                (notPassed, passed) = partition
                /*              val (notPassed, passed) =
                setPartitionMap(values.map(v => (v, checkNodeShapeExprBasic(v, se, rdf))))(mapFun) */
                _ <- info(s"Passed: \n${passed.map(_.toString).mkString(s"\n")}\nNo passed\n${notPassed.map(_.toString).mkString(s"\n")}")
                newt <- if (notPassed.isEmpty) {
                  addEvidence(attempt.nodeShape, s"${node.show} passed ${constraint.show} for path ${path.show}")
                } else
                  err(ValuesNotPassed(attempt, node, path, passed.size, notPassed.toSet))
              } yield newt
              ct
            } else err(ErrCardinality(attempt, node, path, values.size, card))
          } yield t
    }
  }

  private def checkNodeShapeExprBasic(node: RDFNode, se: ShapeExpr, rdf: RDFReader): EitherT[IO, String, String] =
    se match {
      case sa: ShapeAnd => cmb(sa.shapeExprs.map(checkNodeShapeExprBasic(node, _, rdf)))
      case so: ShapeOr  => cmb(so.shapeExprs.map(checkNodeShapeExprBasic(node, _, rdf)))
      case sn: ShapeNot =>
        checkNodeShapeExprBasic(node, sn.shapeExpr, rdf)
          .biflatMap(
            _ => mkOk(s"Passes negation"), 
            _ => mkErr(s"Doesn't pass negation")
            )
      case _: ShapeRef           => mkErr("Internal error. A normalized ShapeExpr cannot have references ")
      case s: Shape if s.isEmpty => mkOk(s"$node matches empty shape")
      case s: Shape              =>
        // checkShapeBase(Attempt(NodeShape(node, ShapeType(s,s.id, schema)),None), node, s)
        mkErr(s"Not implemented yet")
      case _: ShapeExternal   => mkErr(s"Still don't know what to do with external shapes")
      case nk: NodeConstraint => NodeConstraintChecker(validator.schema, rdf).nodeConstraintChecker(node, nk)
    }

  private def mkErr(s: String): EitherT[IO, String, String] =
    EitherT.fromEither(s.asLeft[String])

  private def mkOk(s: String): EitherT[IO, String, String] = EitherT.pure(s)

  private def cmb(els: List[EitherT[IO, String, String]]): EitherT[IO, String, String] = {
    els.sequence.map(_.mkString("\n"))
  }

}
