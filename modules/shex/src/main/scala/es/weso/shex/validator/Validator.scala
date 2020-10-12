package es.weso.shex.validator

import cats._
import implicits._
import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
import es.weso.shex._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.collection.Bag
import es.weso.rbe.interval.IntervalChecker
import es.weso.rbe.{BagChecker, Empty, Rbe}
// import es.weso.rbe.BagChecker._
import es.weso.utils.{SeqUtils, SetUtils}
import es.weso.shex.implicits.showShEx._
import es.weso.rdf.PREFIXES._
import es.weso.shex.validator.Table._
import ShExChecker._
import es.weso.rdf.triples.RDFTriple
import es.weso.shapeMaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMapLabel, _}
import es.weso.shex.actions.TestSemanticAction
import Function.tupled
import es.weso.shex.validator.ShExError._
import es.weso.shex.validator.ConstraintRef.{showConstraintRef => _}


/**
  * ShEx validator
  */
case class Validator(schema: ResolvedSchema,
                     externalResolver: ExternalResolver = NoAction)
    extends ShowValidator(schema)
    with LazyLogging {

  type ShapeChecker     = ShapeExpr => CheckTyping
  type NodeShapeChecker = (RDFNode, Shape) => CheckTyping
  type NodeChecker      = Attempt => RDFNode => CheckTyping
  type Neighs           = List[Arc]
  type Candidates       = List[Candidate]
  type NoCandidates     = List[Arc]
  type Bag_             = Bag[ConstraintRef]
  type Rbe_             = Rbe[ConstraintRef]
  type BagChecker_      = BagChecker[ConstraintRef]
  type ES[A]            = Either[ShExError, A]

  private lazy val `sh:targetNode` = sh + "targetNode"

  private lazy val ignoredPathsClosed: List[Path] =
    List(Inverse(`sh:targetNode`))

  private[validator] def checkTargetNodeDeclarations: CheckTyping =
    for {
      rdf        <- getRDF
      nodeLabels <- getTargetNodeDeclarations(rdf)
      ts <- checkAll(nodeLabels.map {
        case (node, label) => checkNodeLabel(node, label)
      })
      t <- combineTypings(ts)
    } yield t

  private[validator] def checkShapeMap(shapeMap: FixedShapeMap): CheckTyping = {
    checkNodesShapes(shapeMap)
  }

  private[validator] def getTargetNodeDeclarations(rdf: RDFReader): Check[List[(RDFNode, ShapeLabel)]] =
    for {
      ts <- fromStream(rdf.triplesWithPredicate(`sh:targetNode`))
      r  <- checkAll(ts.map(t => (t.obj, mkShapeLabel(t.subj))).toList.map(checkPair2nd))
    } yield r

  private[validator] def checkNodesShapes(fixedMap: FixedShapeMap): CheckTyping =
    for {
      ts <- checkAll(fixedMap.shapeMap.toList.map(tupled(checkNodeShapesMap)))
      t  <- combineTypings(ts)
    } yield t

  private[validator] def checkNodeShapeMapLabel(node: RDFNode, label: ShapeMapLabel, info: Info): CheckTyping =
    info.status match {
      case Conformant =>
        label match {
          case StartMapLabel => checkNodeStart(node)
          case IRIMapLabel(_) | BNodeMapLabel(_) => {
            val lbl = mkLabel(label)
            checkNodeShapeLabel(node, lbl)
          }
        }
      case NonConformant =>
        label match {
          case StartMapLabel =>
            for {
              c     <- checkNodeStart(node)
              check <- checkNotConformant(node, Start, c)
            } yield check
          case IRIMapLabel(_) | BNodeMapLabel(_) => {
            val lbl = mkLabel(label)
            for {
              c     <- checkNodeShapeLabel(node, lbl)
              check <- checkNotConformant(node, lbl, c)
            } yield check
          }
        }
      case Undefined =>
        errStr(s"Cannot check $node against undefined status")
    }

  private def mkLabel(label: ShapeMapLabel): ShapeLabel =
    ShapeLabel.fromShapeMapLabel(label)

  private def checkNotConformant(node: RDFNode, label: ShapeLabel, c: ShapeTyping): CheckTyping =
    if (c.hasNoType(node, label)) ok(c)
    else errStr(s"Node $node should not conform to $label but it does")

  private def checkLabelInfo(node: RDFNode)(pair: (ShapeMapLabel, Info)): CheckTyping = {
    val (label, info) = pair
    checkNodeShapeMapLabel(node, label, info)
  }

  private[validator] def checkNodeShapesMap(node: RDFNode, shapesMap: Map[ShapeMapLabel, Info]): CheckTyping = {
    for {
      ts <- checkAll(shapesMap.map(checkLabelInfo(node)).toList)
      t  <- combineTypings(ts)
    } yield t
  }

  private[validator] def mkShapeLabel(n: RDFNode): Check[ShapeLabel] = {
    n match {
      case i: IRI   => ok(IRILabel(i))
      case b: BNode => ok(BNodeLabel(b))
      case _ => {
        errStr(s"mkShapeLabel: Node ${n.show} can't be a shape")
      }
    }
  }

  private[validator] def getShape(label: ShapeLabel): Check[ShapeExpr] =
    schema.getShape(label) match {
      case Left(e)      => errStr[ShapeExpr](e)
      case Right(shape) => ok(shape)
    }

  private[validator] def checkNodeShapeLabel(node: RDFNode, shape: ShapeLabel): CheckTyping = {
    cond(
      getShapeLabel(shape),
      (shapeLabel: ShapeLabel) => checkNodeLabel(node, shapeLabel),
      err =>
        for {
          t <- getTyping
        } yield t.addNotEvidence(node, ShapeType(ShapeExpr.fail, Some(shape), schema), err)
    )
  }

  private[validator] def checkNodeStart(node: RDFNode): CheckTyping = {
    schema.start match {
      case None => err(NoStart(node))
      case Some(shape) => {
        logger.debug(s"nodeStart. Node: ${node.show}")
        val shapeType = ShapeType(shape, Some(Start), schema)
        val attempt   = Attempt(NodeShape(node, shapeType), None)
        runLocalSafeTyping(checkNodeShapeExpr(attempt, node, shape), _.addType(node, shapeType), (err, t) => {
          t.addNotEvidence(node, shapeType, err)
        })
      }
    }
  }

  private[validator] def getShapeLabel(label: ShapeLabel): Check[ShapeLabel] = {
    if (schema.labels contains label) ok(label)
    else err(LabelNotFound(label, schema.labels))
  }

  private[validator] def checkNodeLabelSafe(node: RDFNode, label: ShapeLabel, shape: ShapeExpr): CheckTyping = {
    val shapeType = ShapeType(shape, Some(label), schema)
    val attempt   = Attempt(NodeShape(node, shapeType), None)
    for {
      _ <- getTyping
      t <- {
        runLocalSafeTyping(
          bind(
            checkOptSemActs(attempt,node, schema.startActs),
            checkNodeShapeExpr(attempt, node, shape)
          ),
          _.addType(node, shapeType),
          (err, t) => {
            t.addNotEvidence(node, shapeType, err)
          }
        )
      }
    } yield {
      t
    }
  }

  private def addNot(node: RDFNode, label: ShapeLabel, typing: ShapeTyping)(err: ShExError): CheckTyping = {
    val shapeType = ShapeType(ShapeExpr.fail, Some(label), schema)
    ok(typing.addNotEvidence(node, shapeType, err))
  }


  private[validator] def checkNodeLabel(node: RDFNode, label: ShapeLabel): CheckTyping = {
    for {
      typing <- getTyping
      newTyping <- if (typing.hasInfoAbout(node, label)) {
        ok(typing)
      } else
        cond(getShape(label), (shape: ShapeExpr) => checkNodeLabelSafe(node, label, shape), addNot(node,label,typing))
    } yield newTyping
  }

  private[validator] def checkNodeShapeExpr(attempt: Attempt, node: RDFNode, s: ShapeExpr): CheckTyping = 
   info(s"CheckNodeShapeExpr $node ") *> {
     s match {
       case so: ShapeOr => checkOr(attempt, node, so.shapeExprs)
       case sa: ShapeAnd => checkAnd(attempt, node, sa.shapeExprs)
       case sn: ShapeNot => checkNot(attempt, node, sn.shapeExpr)
       case nc: NodeConstraint => checkNodeConstraint(attempt, node, nc)
       case s: Shape => checkShape(attempt, node, s)
       case sr: ShapeRef => checkRef(attempt, node, sr.reference)
       case se: ShapeExternal => checkExternal(attempt, node, se)
     }
   }

  private[validator] def checkAnd(attempt: Attempt, node: RDFNode, ses: List[ShapeExpr]): CheckTyping =
    for {
      ts <- checkAll(ses.map(se => checkNodeShapeExpr(attempt, node, se)))
      t  <- combineTypings(ts)
    } yield t

  private[validator] def checkOr(attempt: Attempt, node: RDFNode, ses: List[ShapeExpr]): CheckTyping = {
    val vs = ses.map(se => checkNodeShapeExpr(attempt, node, se))
    for {
      t1 <- checkSome(
        vs,
        StringError(
          s"None of the alternatives of OR(${ses.map(_.showPrefixMap(schema.prefixMap)).mkString(",")}) is valid for node ${node.show}"
        )
      )
      t2 <- addEvidence(attempt.nodeShape, s"${node.show} passes OR")
      t3 <- combineTypings(Seq(t1, t2))
    } yield t3
  }

  private[validator] def checkNot(attempt: Attempt, node: RDFNode, s: ShapeExpr): CheckTyping = {
    val parentShape        = attempt.nodeShape.shape
    val check: CheckTyping = checkNodeShapeExpr(attempt, node, s)
    val handleError: ShExError => Check[ShapeTyping] = e =>
      for {
        t1 <- addNotEvidence(
          NodeShape(node, ShapeType(s, None, schema)),
          e,
          s"${node.show} does not satisfy ${s.show}. Negation declared in ${parentShape.show}. Error: $e"
        )
        t2 <- addEvidence(attempt.nodeShape, s"${node.show} satisfies not(${{ s.show }})")
        t  <- combineTypings(List(t1, t2))
      } yield t
    val handleNotError: ShapeTyping => Check[ShapeTyping] = t =>
      errStr(s"Failed NOT(${s.show}) because node ${node.show} satisfies it")
    cond(check, handleNotError, handleError)
  }

  private def checkNodeHasType(node: RDFNode, shapeLabel: ShapeLabel, typing: ShapeTyping, attempt: Attempt): Check[Unit] =
    if (typing.hasNoType(node, shapeLabel)) {
      typing.getTypingResult(node, shapeLabel) match {
        case None =>
          errStr(s"Node ${node.show} has no shape ${shapeLabel.show}. Attempt: $attempt")
        case Some(tr) =>
          tr.getErrors match {
            case None =>
              errStr(s"Node ${node.show} has no shape ${shapeLabel.show}\nReason typing result ${tr.show} with no errors")
            case Some(es) =>
              errStr(s"Node ${node.show} has no shape ${shapeLabel.show}\nErrors: ${es.map(_.show).mkString("\n")}")
          }
      }
    } else ok(())

  private[validator] def checkRef(attempt: Attempt, node: RDFNode, ref: ShapeLabel): CheckTyping =
    for {
      t <- checkNodeLabel(node, ref)
      _ <- checkNodeHasType(node,ref,t, attempt)
    } yield t

  private[validator] def checkNodeConstraint(attempt: Attempt, node: RDFNode, s: NodeConstraint): CheckTyping =
    for {
      t1 <- optCheck(s.nodeKind, checkNodeKind(attempt, node), getTyping)
      t2 <- optCheck(s.values, checkValues(attempt, node), getTyping)
      t3 <- optCheck(s.datatype, checkDatatype(attempt, node), getTyping)
      t4 <- checkXsFacets(attempt, node)(s.xsFacets)
      t  <- combineTypings(List(t1, t2, t3, t4))
    } yield {
      t
    }

  private [validator] def getExternalShape(se: ShapeExternal): Check[ShapeExpr] = se.id match {
    case None => errStr(s"No label in external shape")
    case Some(label) => fromIO(externalResolver.getShapeExpr(label, se.annotations))
  }


  private[validator] def checkExternal(attempt: Attempt,
                                       node: RDFNode,
                                       se: ShapeExternal): CheckTyping = {
    for {
      externalShape <- getExternalShape(se)
      newAttempt = Attempt(NodeShape(node, ShapeType(externalShape, se.id, schema)), attempt.path)
      t <- checkNodeShapeExpr(newAttempt, node, externalShape)
    } yield t
  }

  private[validator] def checkValues(attempt: Attempt,
                                     node: RDFNode)
                                    (values: List[ValueSetValue]): CheckTyping = {
    val cs: List[CheckTyping] =
      values.map(v => ValueChecker(schema).checkValue(attempt, node)(v))
    checkSome(cs, StringError(s"${node.show} does not belong to [${values.map(_.show).mkString(",")}]"))
  }

  private[validator] def checkDatatype(attempt: Attempt, node: RDFNode)(datatype: IRI): CheckTyping =
    for {
      rdf   <- getRDF
      hasDatatype <- fromIO(rdf.checkDatatype(node, datatype))
      check <- checkCond(
        hasDatatype,
        attempt,
        CheckDatatypeError(node, datatype),
        s"${node.show} has datatype ${datatype.show}")
    } yield check

  private[validator] def checkXsFacets(attempt: Attempt, node: RDFNode)(xsFacets: List[XsFacet]): CheckTyping = {
    if (xsFacets.isEmpty) getTyping
    else
      for {
        rdf <- getRDF
        t   <- FacetChecker(schema, rdf).checkFacets(attempt, node)(xsFacets)
      } yield t
  }

  private[validator] def checkNodeKind(attempt: Attempt, node: RDFNode)(nk: NodeKind): CheckTyping = {
    nk match {
      case IRIKind =>
        checkCond(node.isIRI, attempt, StringError(s"${node.show} is not an IRI"), s"${node.show} is an IRI")
      case BNodeKind =>
        checkCond(
          node.isBNode,
          attempt,
          StringError(s"${node.show} is not a BlankNode"),
          s"${node.show} is a BlankNode"
        )
      case NonLiteralKind =>
        checkCond(
          !node.isLiteral,
          attempt,
          StringError(s"${node.show} is a literal but should be a NonLiteral"),
          s"${node.show} is NonLiteral"
        )
      case LiteralKind =>
        checkCond(node.isLiteral, attempt, StringError(s"${node.show} is not an Literal"), s"${node.show} is a Literal")
    }
  }

  private[validator] def checkShape(attempt: Attempt, node: RDFNode, s: Shape): CheckTyping =
    info(s"CheckShape $node") *>
    (s._extends match {
      case None     => checkShapeBase(attempt, node, s)
      case Some(es) => checkShapeExtendLs(attempt, node, s, es)
    })

  private[validator] def checkShapeExtendLs(
      attempt: Attempt,
      node: RDFNode,
      s: Shape,
      es: List[ShapeLabel]
  ): CheckTyping = {
    es match {
      case Nil      => checkShapeBase(attempt, node, s)
      case e :: Nil => checkShapeExtend(attempt, node, s, e)
      case _        => errStr(s"Multiple inheritance not supported yet")
    }
  }

  private[validator] def checkShapeExtend(
      attempt: Attempt,
      node: RDFNode,
      s: Shape,
      baseLabel: ShapeLabel
  ): CheckTyping =
    for {
      base  <- getShape(baseLabel)
      paths <- fromEither(s.paths(schema).leftMap(StringError(_)))
      // _ <- { println(s"checkShapeExtend(node=$node,shape=${s.show},base=$baseLabel). \npaths=$paths") ; ok(()) }
      neighs <- getNeighPaths(node, paths.toList)
      partitions = SetUtils.pSet(neighs.toSet)
      _      <- checkSomeFlag(partitions, checkPartition(base, s, attempt, node), noPartition(attempt, node, s, baseLabel, neighs))
      typing <- getTyping
    } yield typing

  private[validator] def checkPartition(base: ShapeExpr, s: Shape, attempt: Attempt, node: RDFNode)(
      pair: (Set[Arc], Set[Arc])
  ): Check[(ShapeTyping, Boolean)] = {
    val (neighs1, neighs2) = pair
    // println(s"Checking partition ($neighs1,$neighs2)\n$neighs1 with ${base.show}\nand\n$neighs2 with ${s.show}")
    (for {
      pair <- checkNeighsShapeExpr(attempt, node, neighs1.toList, base)
      (typing1, flag) = pair
      // _ <- { println(s"Typing1: $typing1"); ok(()) }
      typing2 <- checkNeighsShape(attempt, node, neighs2.toList, s)
      // _ <- { println(s"Typing2: $typing2"); ok(()) }
    } yield (typing2, true)) orElse
      (for {
        // _ <- {println(s"partition ($neighs1,$neighs2) failed"); ok(()) }
        t <- getTyping
      } yield (t, false))
  }

  /*  private def orElseDebug[A](c1: Check[A], c2:Check[A]): Check[A] = {
    c1.orElse(c2)
  } */

  private[validator] def noPartition(
     attempt: Attempt, 
     node: RDFNode, 
     s: Shape, 
     label: ShapeLabel,
     neighs: Neighs
     ): Check[(ShapeTyping, Boolean)] =
     errStr(s"No partition of $neighs conforms. Node: $node")

  private[validator] def checkNeighsShapeExpr(
      attempt: Attempt,
      node: RDFNode,
      neighs: Neighs,
      se: ShapeExpr
  ): Check[(ShapeTyping, Boolean)] =
    se match {
      case s: Shape =>
        (for {
          t <- checkNeighsShape(attempt, node, neighs, s)
          // _ <- { println(s"Failed checkNeighsShape(node=${node.show}, neighs=$neighs, se=${se.show} passed with $t") ; ok(())}
        } yield (t, true)) orElse {
          for {
            // _ <- { println(s"Failed checkNeighsShape(node=${node.show}, neighs=$neighs, se=${se.show} failed") ; ok(())}
            t <- getTyping
          } yield (t, false)
        }
      case _ => {
        // println(s"Not implemented yet extends with a non shape base: $se")
        errStr(s"Not implemented yet extends with a non shape base: $se")
      }
    }

  private[validator] def checkNeighsShape(attempt: Attempt, node: RDFNode, neighs: Neighs, s: Shape): CheckTyping =
    if (s.hasRepeatedProperties(schema))
      checkNeighsShapeWithTable(attempt, node, neighs, s)
    else {
      // TODO
      checkNeighsShapeWithTable(attempt, node, neighs, s)
    }

  private[validator] def checkNeighsShapeWithTable(
      attempt: Attempt,
      node: RDFNode,
      neighs: Neighs,
      s: Shape
  ): CheckTyping = {
    for {
      tableRbe <- mkTable(s.expression, s.extra.getOrElse(List()), schema.prefixMap)
      (cTable, rbe) = tableRbe
      _ <- info(s"cTable: $cTable")
      bagChecker    = IntervalChecker(rbe)
      csRest <- calculateCandidates(neighs, cTable)
      (candidates, rest) = csRest
      _     <- checkRests(rest, s.extraPaths, s.isClosed, ignoredPathsClosed)
      paths <- fromEither(s.paths(schema).leftMap(StringError(_)))
      _ <- {
        if (s.isClosed) {
          checkNoStrangeProperties(node, paths.toList, attempt)
        } else ok(())
      }
      _ <- info(s"Before checkCandidates:\n ${candidates.map(_.show).mkString(",")}\nTable:${cTable.show}\n")
      typing <- checkCandidates(attempt, bagChecker, cTable)(candidates)
      _ <- info(s"After checkCandidates: $typing")
      _ <- checkOptSemActs(attempt,node, s.actions)
    } yield {
      // println(s"End of checkShape(attempt=${attempt.show},node=${node.show},shape=${s.show})=${typing.show}")
      typing
    }
  }

  private def getPaths(s: Shape): Check[List[Path]] =
    fromEitherString(s.paths(schema).map(_.toList))

  private[validator] def checkShapeBase(attempt: Attempt, node: RDFNode, s: Shape): CheckTyping = {
    info(s"CheckShapeBase $node FlatShape? ${s.isFlatShape(schema)}") *> {
    s match {
      case _ if s.isEmpty => addEvidence(attempt.nodeShape, s"Node $node matched empty shape")
      case _ if s.isFlatShape(schema) =>
        for {
          flatShape <- fromEitherString(s.flattenShape(schema))
          typing    <- ValidateFlatShape(this).checkFlatShape(attempt, node, flatShape)
        } yield typing
      case _ =>
        for {
          paths <- getPaths(s)  
          _ <- info(s"getNeighPaths: node=${node.show}\n Paths:\n${paths.map(_.show).mkString(",")}\n")
          neighs <- getNeighPaths(node, paths)
          _ <- info(s"Neighs: $neighs")
          typing <- checkNeighsShape(attempt, node, neighs, s)
        } yield typing
    }
  }
}

  private def checkNoStrangeProperties(node: RDFNode, paths: List[Path], attempt: Attempt): Check[Unit] =
    for {
      s   <- getNotAllowedPredicates(node, paths)
      _ <- checkCond(s.isEmpty, attempt, ExtraPropertiesClosedShape(node,s.toList), "Closed properties with no extra property")
    } yield ()

  private[validator] def checkOptSemActs(attempt: Attempt, node: RDFNode, maybeActs: Option[List[SemAct]]): Check[Unit] =
    maybeActs match {
      case None     => ok(())
      case Some(as) => checkSemActs(attempt,node, as)
    }

  private[validator] def checkSemActs(attempt: Attempt, node: RDFNode, as: List[SemAct]): Check[Unit] =
    for {
      _ <- checkAll(as.map(checkSemAct(attempt,node, _)))
    } yield ()

  private[validator] def checkSemAct(attempt: Attempt, node: RDFNode, a: SemAct): Check[Unit] =
    for {
      rdf <- getRDF
      _ <- info("Before check semantic action")
      eitherResult   <- {
        println(s"Before running action")
        runAction(a.name, a.code, node, rdf)
      }
      _ <- info("After check semantic action")
      _ <- fromEither(eitherResult.leftMap(exc => SemanticActionException(attempt, node, a, exc)))
    } yield ()

  private[validator] def runAction(name: IRI, code: Option[String], node: RDFNode, rdf: RDFReader): Check[Either[Throwable,Unit]] = {
    // println(s"Semantic action: $name/$code")
    for {
      r <- name match {
        case TestSemanticAction.`iri` => {
          println(s"Before TestSemanticAction runAction")
          fromIO(TestSemanticAction.runAction(code.getOrElse(""), node, rdf).attempt)
        }
        case _ => {
          logger.info(s"Unsupported semantic action processor: $name")
          addLog(List(Action(name, code)))
          ok(Right(()))
        }
      }
    } yield r
  }


  private[validator] def checkRests(
      rests: List[Arc],
      extras: List[Path],
      isClosed: Boolean,
      ignoredPathsClosed: List[Path]
  ): Check[Unit] = {
    val zero: Either[String, Unit] = Right(())
    def combine(step: Either[String, Unit], current: Either[String, Unit]): Either[String, Unit] =
      (step, current) match {
        case (Left(str1), _)        => Left(str1)
        case (_, Left(str2))        => Left(str2)
        case (Right(()), Right(())) => Right(())
      }
    val ts: List[Either[String, Unit]] = rests.map(checkRest(_, extras, isClosed, ignoredPathsClosed))
    val r: Either[String, Unit]        = ts.foldLeft(zero)(combine)
    r.fold(e => errStr(e), _ => ok(()))
  }

  private[validator] def checkRest(
      rest: Arc,
      extras: List[Path],
      isClosed: Boolean,
      ignoredPathsClosed: List[Path]
  ): Either[String, Unit] = {
    val restPath = rest.path
    // Ignore extra predicates if they are inverse
    if (isClosed && restPath.isDirect) {
      // TODO: Review if the extra.contains(restpath) check is necessary
      // Extra has been implemented as a negation
      if (ignoredPathsClosed.contains(restPath) || extras.contains(restPath)) {
        Right(())
      } else {
        Left(
          s"Closed shape. But rest ${restPath.show} is not in ${ignoredPathsClosed.map(_.show).mkString(",")} or ${extras.map(_.show).mkString(",")}"
        )
      }
    } else Right(())
  }

  private[validator] def mkTable(maybeTe: Option[TripleExpr], extra: List[IRI], prefixMap: PrefixMap): Check[(CTable, Rbe_)] = {
    maybeTe match {
      case None => ok((CTable.empty, Empty))
      case Some(te) =>
        fromEitherString(
          for {
            pair <- CTable.mkTable(te, extra, schema.tripleExprMap, prefixMap)
          } yield pair
        )
    }
  }

  /**
    * Calculates the sequence of candidates
    * Example: Neighs (p,x1),(p,x2),(q,x2),(r,x3)
    *   Table: { constraints: C1 -> IRI, C2 -> ., paths: p -> List(C1,C2), q -> C1 }
    *   Result: x1
    * @param neighs
    * @param table
    * @return a tuple (cs,rs) where cs is the list of candidates and rs is the nodes that didn't match any
    */
  private[validator] def calculateCandidates(neighs: Neighs, table: CTable): Check[(Candidates, NoCandidates)] = {
    val candidates = table.neighs2Candidates(neighs)
    val (cs, rs)   = candidates.partition(matchable)
    // println(s"Candidates partitioned: cs:\n${cs.map(_.show).mkString(s"\n")}\nrs:${rs.map(_.show).mkString(s"\n")}\n")
    ok((cs, rs.map(_.arc)))
  }

  private def matchable(c: Candidate): Boolean = {
    c.crefs.nonEmpty
  }



  private[validator] def checkCandidates(attempt: Attempt, bagChecker: BagChecker_, table: CTable)(
      cs: Candidates
  ): CheckTyping = {
    // println(s"checkCandidates: Candidates: $cs")
    val as: List[CandidateLine] = SeqUtils.transpose(cs.map(c => (c.arc, c.crefs))).map(CandidateLine(_))

    // println(s"Candidate lines: $as, ${as.length}")
    as.length match {
      case 1 => { // Deterministic
        checkCandidateLine(attempt, bagChecker, table)(as.head)
      }
      case 0 => {
        errStr(s"${attempt.show} Empty list of candidates")
      }
      case n => {
        // println(s"Non deterministic")
        val checks: List[CheckTyping] =
          as.map(checkCandidateLine(attempt, bagChecker, table)(_))
        checkSome(checks, NoCandidate(attempt,bagChecker,as,table))
      }
    }
  }

  private[validator] def checkCandidateLine(attempt: Attempt, bagChecker: BagChecker_, table: CTable)(
      cl: CandidateLine
  ): CheckTyping = {
    // println(s"checkCandidateLine: ${cl}")
    // println(s"Table: $table")
    val bag = cl.mkBag
    
    val s = implicitly[Show[ConstraintRef]]
    println(s"Before check candidateline $s")
    bagChecker
      .check(bag, false)
      .fold(
        e => {
          // println(s"Does not match RBE. ${bag} with ${bagChecker.show}")
          err(ErrRBEMatch(attempt,cl,table,bag,bagChecker.rbe,e.head))
/*          errStr(s"${attempt.show} Candidate line ${showCandidateLine(cl,table)} which corresponds to ${bag} does not match ${Rbe
            .show(bagChecker.rbe)}\nTable:${table.show}\nErr: $e") */
        },
        bag => {
          // println(s"Matches RBE...")
          val nodeConstraints = cl.nodeConstraints(table)
          val checkNodeConstraints: List[CheckTyping] =
            nodeConstraints.map {
              case (node, pair) => {
                val (shapeExpr, maybeSemActs) = pair
                // println(s"Checking $node with $shapeExpr")
                for {
                  t <- checkNodeShapeExpr(attempt, node, shapeExpr)
                  _ <- checkOptSemActs(attempt, node, maybeSemActs)
                } yield t
              }
            }
          for {
            typing <- getTyping
            ts     <- checkAll(checkNodeConstraints)
            t      <- combineTypings(typing :: ts)
          } yield {
            t
          }
        }
      )
  }

  private[validator] def getNeighs(node: RDFNode): Check[Neighs] =
    for {
      rdf        <- getRDF
      outTriples <- fromStream(rdf.triplesWithSubject(node))
      outgoing = outTriples.map(t => Arc(Direct(t.pred), t.obj)).toList
      inTriples <- fromStream(rdf.triplesWithObject(node))
      incoming = inTriples.map(t => Arc(Inverse(t.pred), t.subj)).toList
    } yield {
      val neighs = outgoing ++ incoming
      neighs
    }

  private[validator] def getNeighPaths(node: RDFNode, paths: List[Path]): Check[Neighs] = {
    val outgoingPredicates = paths.collect { case Direct(p) => p }
    for {
      rdf        <- getRDF
      _ <- info(s"getNeighPaths\ntriplesWithSubjectPredicates(${node.show}, OutgoingPreds = ${outgoingPredicates.map(_.show).mkString(",")})")
      outTriples <- fromIO(getTriplesWithSubjectPredicates(rdf,node,outgoingPredicates))
      _ <- info(s"getNeighPaths\ntriplesWithSubjectPredicates(${node.show}, ${outgoingPredicates.map(_.show).mkString(",")}): Outtriples: ${outTriples.map(_.show).mkString("|")}\nNode: $node\nPreds:$outgoingPredicates")
      strRdf <- fromIO(rdf.serialize("TURTLE"))
      _ <- info(s"RDF: ${strRdf}")
      outgoing = outTriples.map(t => Arc(Direct(t.pred), t.obj)).toList
      inTriples <- fromStream(rdf.triplesWithObject(node))
      incoming = inTriples.map(t => Arc(Inverse(t.pred), t.subj)).toList
    } yield {
      val neighs = outgoing ++ incoming
      neighs
    }
  }

  private def getTriplesWithSubjectPredicates(rdf: RDFReader, node: RDFNode, preds: List[IRI]): IO[List[RDFTriple]] = {
    // println(s"GetTriplesWithSubjectPredicate...${node.show} \nPreds=${preds.map(_.show)}")
    node match {
      case _: IRI => { 
       // println(s"IRI...$node")
        val vs = triplesWithSubjectPredicates(node,preds,rdf)
       // println(s"Triples obtained: ${vs.unsafeRunSync()}")
       // println(s"Triples for node: ${rdf.triplesWithSubject(node).compile.toList.unsafeRunSync()}")
       // println(s"Preds: $preds")
        //preds.map(p => 
        //  println(s"Triples for ${node.show}/${p.show}: ${rdf.triplesWithSubjectPredicate(node,p).compile.toList.unsafeRunSync()}")
        //) 
        vs
      }
      case _: BNode => triplesWithSubjectPredicates(node,preds,rdf)
      case _ => { 
        // println(s"Literal node? ${node}")
        IO(List())
      }
    }
  }

  private def triplesWithSubjectPredicates(n: RDFNode, ps: List[IRI], rdf: RDFReader): IO[List[RDFTriple]] = {
    // println(s"TriplesWithSubjectPredicates ${n.show}, ${ps.map(_.show).mkString(",")}")
    val ss = mkSeq(ps, (p: IRI) => {
      // println(s"TriplesWithSubjectPredicates ${n.show}/${p.show}")
      val ts: IO[List[RDFTriple]] = rdf.triplesWithSubjectPredicate(n,p).compile.toList
      // println(s"TriplesWithSubjectPredicates ${n.show}/${p.show} = ${ts.unsafeRunSync().map(_.show).mkString(",")}")
      ts
    })
    ss
  } 

  private def mkSeq[A,B](vs: List[A], f: A => IO[List[B]]): IO[List[B]] = {
    vs.traverse(f).map((_.flatten))
  }

  private[validator] def getNotAllowedPredicates(node: RDFNode, paths: List[Path]): Check[Set[IRI]] =
    for {
      rdf <- getRDF
      ts  <- fromStream(rdf.triplesWithSubject(node))
    } yield {
      val allowedPreds = paths.collect { case Direct(p) => p }
      ts.toSet[RDFTriple].collect {
        case s if !(allowedPreds contains s.pred) => s.pred
      }
    }

  def validateNodeStart(rdf: RDFReader, node: IRI): IO[Result] = {
    runValidator(checkNodeStart(node), rdf)
  }

  def validateShapeMap(rdf: RDFReader, shapeMap: FixedShapeMap): IO[Result] = {
    runValidator(checkShapeMap(shapeMap), rdf)
  }

  def runValidator(chk: Check[ShapeTyping], rdf: RDFReader): IO[Result] = for {
    r <- runCheck(chk, rdf)
    pm <- rdf.getPrefixMap
  } yield cnvResult(r, rdf, pm)

  private def cnvResult(r: CheckResult[ShExError, ShapeTyping, Log], rdf: RDFReader, rdfPrefixMap: PrefixMap): Result = Result (
    for {
      shapeTyping <- r.toEither
      result      <- shapeTyping.toShapeMap(rdfPrefixMap, schema.prefixMap).leftMap(StringError)
    } yield result
  )

}

object Validator {

  def empty: Validator = Validator(schema = ResolvedSchema.empty)

  def validate(schema: ResolvedSchema, fixedShapeMap: FixedShapeMap, rdf: RDFReader): IO[Result] = {
    val validator = Validator(schema)
    validator.validateShapeMap(rdf, fixedShapeMap)
  }

}
