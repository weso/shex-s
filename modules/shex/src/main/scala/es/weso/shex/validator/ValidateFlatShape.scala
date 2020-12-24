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
// import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.RDFReader
import es.weso.utils.eitherios.EitherIOUtils._
import es.weso.shex.validator.ShExError._
import es.weso.rdf.PrefixMap

/**
  * ShEx validator
  */
case class ValidateFlatShape(
  validator: Validator,
  nodesPrefixMap: PrefixMap,
  shapesPrefixMap: PrefixMap
) {

  private[validator] def checkFlatShape(
    attempt: Attempt, 
    node: RDFNode, 
    shape: FlatShape): CheckTyping = {
    val zero = getTyping
    def cmb(checkTyping: CheckTyping, 
            slot: (Path, Constraint)
            ): CheckTyping = {
      val (path, constraint) = slot
      for {
        _ <- { info(s"""|CheckFlatShape 
                        |  Slot path=${path.show}, 
                        |  constraint=${constraint.show}
                        |  Attempt: ${attempt}
                        |""".stripMargin) }
        typing1 <- checkTyping
        typing2 <- checkConstraint(attempt, node, path, constraint)
        typing  <- combineTypings(typing1, typing2)
      } yield {
        typing
      }
    }
    for {
      _ <- info(s"""|FlatShape 
                    | shape: ${shape.show}
                    | node: ${node.show}
                    |""".stripMargin)
      extra <- extraPreds(node, shape.preds)
      _ <- info(s"Extra preds: $extra. Closed? ${shape.closed}")
      typing <- if (shape.closed && extra.nonEmpty) {
        err(ClosedButExtraPreds(extra))  
        // TODO: Not sure about this check
      } else 
        shape.slots.foldLeft(zero)(cmb)
      _ <- info(s"FlatShape(${node.show}@${shape.show}}) successful")  
    } yield typing
  }

  // Returns the list of paths that are different from a given list
  private def extraPreds(node: RDFNode, preds: Set[IRI]): Check[Set[IRI]] =
    for {
      existingPreds <- getExistingPredicates(node)
    } yield existingPreds -- preds

  private def checkConstraint(attempt: Attempt, node: RDFNode, path: Path, constraint: Constraint): CheckTyping =
    for {
      _ <- info(s"checkConstraint: ${constraint.show} for ${showNode(node)} with path ${path.show}")
      values <- getValuesPath(node, path)
      _ <- info(s"Values of node ${showNode(node)} with path ${path.show} = [${values.map(_.show).mkString(",")}]")
      typing <- checkValuesConstraint(values, constraint, node, path, attempt)
      _ <- info(s"After checkConstraint: typing = ${typing.show}")
    } yield typing

  private def getExistingPredicates(node: RDFNode): Check[Set[IRI]] =
    getNeighs(node).map(_.getPredicates())


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
        if (card.contains(values.size)) addEvidence(attempt.nodeShape, s"# of values fits $card")
        else for {
          rdf <- getRDF
          _ <- info(s"Cardinality error ${values.size} $card") 
          r <- err[ShapeTyping](ErrCardinality(attempt, node, path, values.size, card,rdf))
        } yield r
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
                    s"Number of values for ${showNode(node)} with ${path.showQualified(nodesPrefixMap)} that satisfy ${constraint.shape} = ${passed.size} matches cardinality ${constraint.card}"
                  )
                } else {
                  info(s"Cardinality with Extra: ${passed.size} ${card}") >>
                  err(ErrCardinalityWithExtra(attempt, node, path, passed.size, notPassed.size, card,rdf))
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
                  addEvidence(attempt.nodeShape, s"${showNode(node)} passed ${constraint.showQualified(shapesPrefixMap)} for path ${path.showQualified(nodesPrefixMap)}")
                } else
                  err[ShapeTyping](ValuesNotPassed(attempt, node, path, passed.size, notPassed.toSet,rdf))
              } yield newt
              ct
            } else 
             info(s"Cardinality error: ${values.size}<>${card}") >>
             err(ErrCardinality(attempt, node, path, values.size, card,rdf))
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
        mkErr(s"checkNodeShapeExprBasic: Not implemented yet Shape ")
      case _: ShapeExternal   => mkErr(s"Still don't know what to do with external shapes")
      case nk: NodeConstraint => NodeConstraintChecker(validator.schema, rdf).nodeConstraintChecker(node, nk)
      case sd: ShapeDecl => mkErr(s"checkNodeShapeExprBasic: Not implemented yet ShapeDecl($sd)")
      case _ => mkErr(s"checkNodeShapeExprBasic: Not implemented yet ShapeDecl($se)")
    }

  private def mkErr(s: String): EitherT[IO, String, String] =
    EitherT.fromEither(s.asLeft[String])

  private def mkOk(s: String): EitherT[IO, String, String] = EitherT.pure(s)

  private def cmb(els: List[EitherT[IO, String, String]]): EitherT[IO, String, String] = {
    // val rs : EitherT[IO,String,List[String]] = 
    els.sequence.map(_.mkString("\n"))
  }

  def showNode(node: RDFNode): String = nodesPrefixMap.qualify(node)
  def showShape(shapeLabel: ShapeLabel): String = shapesPrefixMap.qualify(shapeLabel.toRDFNode)

}
