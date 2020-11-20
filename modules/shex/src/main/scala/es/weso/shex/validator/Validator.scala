package es.weso.shex.validator

import cats._
import implicits._
import cats.effect.IO
import com.typesafe.scalalogging.LazyLogging
import es.weso.shex._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rbe.interval.IntervalChecker
import es.weso.rbe.Empty
import es.weso.utils.{SeqUtils, SetUtils}
import es.weso.shex.implicits.showShEx._
import ShExChecker._
// import es.weso.rdf.triples.RDFTriple
import es.weso.shapeMaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMapLabel, _}
import es.weso.shex.actions.TestSemanticAction
import Function.tupled
import es.weso.shex.validator.ShExError._
import es.weso.shex.validator.ConstraintRef.{showConstraintRef => _}
import ValidationUtils._

/**
  * ShEx validator
  */
case class Validator(schema: ResolvedSchema,
                     externalResolver: ExternalResolver = NoAction,
                     builder: RDFBuilder
                     )
    extends ShowValidator(schema)
    with LazyLogging {

  type ShapeChecker     = ShapeExpr => CheckTyping
  type NodeShapeChecker = (RDFNode, Shape) => CheckTyping
  type NodeChecker      = Attempt => RDFNode => CheckTyping


  private def checkTargetNodeDeclarations: CheckTyping =
    for {
      rdf        <- getRDF
      nodeLabels <- getTargetNodeDeclarations(rdf)
      ts <- checkAll(nodeLabels.map {
        case (node, label) => checkNodeLabel(node, label)
      })
      t <- combineTypings(ts)
    } yield t

  private def checkShapeMap(shapeMap: FixedShapeMap): CheckTyping = {
    checkNodesShapes(shapeMap)
  }

  private def checkNodesShapes(fixedMap: FixedShapeMap): CheckTyping =
    for {
      ts <- checkAll(fixedMap.shapeMap.toList.map(tupled(checkNodeShapesMap)))
      t  <- combineTypings(ts)
      nodesPrefixMap <- getNodesPrefixMap
      _ <- info(s"end of checkNodeShapes: ${t.showShort(nodesPrefixMap,schema.prefixMap)}")
      //newT <- removeAbstractShapes(t)
      // _ <- info(s"after removing abstract shapes: ${newT.showShort(nodesPrefixMap,schema.prefixMap)}")
    } yield t // newT

/*  private def removeAbstractShapes(t: ShapeTyping): CheckTyping = {
    ok(t.negateShapeTypesWith(_.isAbstract, AbstractShapeErrNoArgs()))
  }  */

  private def checkNodeShapeMapLabel(node: RDFNode, label: ShapeMapLabel, info: Info): CheckTyping =
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

  private def checkNotConformant(node: RDFNode, label: ShapeLabel, c: ShapeTyping): CheckTyping =
    if (c.hasNoType(node, label)) ok(c)
    else errStr(s"Node $node should not conform to $label but it does")

  private def checkLabelInfo(node: RDFNode)(pair: (ShapeMapLabel, Info)): CheckTyping = {
    val (label, info) = pair
    checkNodeShapeMapLabel(node, label, info)
  }

  private def checkNodeShapesMap(node: RDFNode, shapesMap: Map[ShapeMapLabel, Info]): CheckTyping = {
    for {
      ts <- checkAll(shapesMap.map(checkLabelInfo(node)).toList)
      t  <- combineTypings(ts)
    } yield t
  }

  private def getShape(label: ShapeLabel): Check[ShapeExpr] =
    schema.getShape(label) match {
      case Left(e)      => err(ShExError.LabelNotFound(label,schema.labels))
      case Right(shape) => ok(shape)
    }

  private def checkNodeShapeLabel(node: RDFNode, shape: ShapeLabel): CheckTyping = {
    cond(verifyShapeLabel(shape),
      (shapeLabel: ShapeLabel) => checkNodeLabel(node, shapeLabel),
      err => for {
          t <- getTyping
        } yield t.addNotEvidence(node, ShapeType(ShapeExpr.fail, Some(shape), schema), err)
    )
  }

  private def checkNodeStart(node: RDFNode): CheckTyping = {
    schema.start match {
      case None => err(NoStart(node))
      case Some(shape) => {
        val shapeType = ShapeType(shape, Some(Start), schema)
        val attempt   = Attempt(NodeShape(node, shapeType), None)
        runLocalSafeTyping(checkNodeShapeExpr(attempt, node, shape), _.addType(node, shapeType), (err, t) => {
          t.addNotEvidence(node, shapeType, err)
        })
      }
    }
  }

  private def verifyShapeLabel(label: ShapeLabel): Check[ShapeLabel] = {
    if (schema.labels contains label) ok(label)
    else err(LabelNotFound(label, schema.labels))
  }

  private def checkNodeLabelSafe(node: RDFNode, label: ShapeLabel, shape: ShapeExpr): CheckTyping = {
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
      _ <- info(s"checkNodeLabel(${node.show},${label.show})")
      strInheritance <- fromIO(schema.inheritanceGraph.show(label => schema.prefixMap.qualify(label.toRDFNode)))
      _ <- info(s"InheritanceGraph: ${strInheritance}")
      typing <- getTyping
      newTyping <- if (typing.hasInfoAbout(node, label)) {
        info(s"Typing contains label ${label.show}") >>
        ok(typing)
      } else
        cond(getShape(label), 
          (shape: ShapeExpr) => checkNodeLabelSafe(node, label, shape), 
          addNot(node,label,typing)
        )
      nodesPrefixMap <- getNodesPrefixMap  
      _ <- infoTyping(newTyping,s"Result of checkNodeLabel(${node.show},${label.show})",nodesPrefixMap)  
    } yield newTyping
  }

  private def sh(s: Set[ShapeLabel]): String = 
    if (s.isEmpty) "{}" 
    else s.map(_.show).mkString(",")

  private def checkDescendants(node: RDFNode, 
    s: ShapeExpr, 
    attempt: Attempt
   ): CheckTyping = for {
     descendants <- fromIO(s.id.map(schema.inheritanceGraph.ancestors(_)).getOrElse(IO(Set[ShapeLabel]())))
     visited <- getVisited
     filteredDescendants = descendants.diff(visited)
     _ <- info(s"Check descendants=${sh(descendants)}, visited=${sh(visited)}- filtered=${sh(filteredDescendants)} ")
     t <- if (filteredDescendants.isEmpty) 
        info(s"Empty filter") >>
        err(NoDescendant(node,s,attempt))
      else 
       checkSome(descendants.toList.map(d => checkNodeLabel(node,d)), NoDescendant(node,s,attempt))
     _ <- info(s"Success of descendants...")
  } yield t

  private def checkNodeShapeExpr(
     attempt: Attempt, 
     node: RDFNode, 
     s: ShapeExpr): CheckTyping = {
   val shapesPrefixMap = schema.prefixMap
   val shapeExprShown  = s.id.map(sl => shapesPrefixMap.qualify(sl.toRDFNode)).getOrElse(s.show)
   checkDescendants(node, s, attempt) orElse (for {
     nodesPrefixMap <- getNodesPrefixMap
     _ <- info(s"checkNodeShapeExpr(${node.show},${shapeExprShown})")
     typing <- s match {
       case so: ShapeOr => checkOr(attempt, node, so.shapeExprs)
       case sa: ShapeAnd => checkAnd(attempt, node, sa.shapeExprs)
       case sn: ShapeNot => checkNot(attempt, node, sn.shapeExpr)
       case nc: NodeConstraint => checkNodeConstraint(attempt, node, nc)
       case s: Shape => 
        checkShape(attempt, node, s)
       case sr: ShapeRef => checkRef(attempt, node, sr.reference)
       case se: ShapeExternal => checkExternal(attempt, node, se)
       case sd: ShapeDecl => 
         info(s"checkShapeDecl(${node.show}, ${sd})") *>
         checkShapeDecl(attempt,node, sd)
       // case _ => errStr(s"checkNodeShapeExpr: Unsupported type of ShapeExpr: ${s}")
     }
     _ <- info(s"end of checkNodeShapeExpr(${node.show},${shapeExprShown}): typing = ${typing.showShort(nodesPrefixMap, shapesPrefixMap)}")
   } yield typing)
  }

  private def checkAnd(attempt: Attempt, node: RDFNode, ses: List[ShapeExpr]): CheckTyping =
    for {
      ts <- checkAll(ses.map(se => checkNodeShapeExpr(attempt, node, se)))
      t  <- combineTypings(ts)
    } yield t

  private def checkOr(attempt: Attempt, node: RDFNode, ses: List[ShapeExpr]): CheckTyping = {
    val vs = ses.map(se => checkNodeShapeExpr(attempt, node, se))
    for {
      t1 <- checkSome(
        vs,
        StringError(
          s"None of the alternatives of OR(${ses.map(_.showQualified(schema.prefixMap)).mkString(",")}) is valid for node ${node.show}"
        )
      )
      t2 <- addEvidence(attempt.nodeShape, s"${node.show} passes OR")
      t3 <- combineTypings(t1, t2)
    } yield t3
  }

  private def checkNot(attempt: Attempt, node: RDFNode, s: ShapeExpr): CheckTyping = {
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

  private def checkShapeDecl(attempt: Attempt, node: RDFNode, sd: ShapeDecl): CheckTyping = for {
    t <- checkNodeShapeExpr(attempt,node,sd.shapeExpr)
  } yield if (sd._abstract) {

    // t.addNotEvidence(node,ShapeType(sd,sd.id,schema), AbstractShapeErr(node,sd))
    t 
    // If sd is abstract then node should not conform to it
    // However, we keep the node@sd association here and remove it when reporting the result
    // The goal of maintaining here the association is for cases like
    // abstract <A> { }
    // <B> @<A> 
    // When validating node@<B> we ask for node@<A> and node@<A> is required for node to conform to <B>
  } else t

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
    s._extends match {
      case None     => checkShapeBase(attempt, node, s)
      case Some(es) => checkShapeExtendLs(attempt, node, s, es)
    }

  private[validator] def checkShapeExtendLs(
      attempt: Attempt,
      node: RDFNode,
      s: Shape,
      es: List[ShapeLabel]
  ): CheckTyping = {
    es match {
      case Nil      => checkShapeBase(attempt, node, s)
      case e :: Nil => checkShapeExtend(attempt, node, s, e)
      case e :: rs  => for {
        t1 <- checkShapeExtend(attempt,node,s,e)
        t2 <- checkShapeExtendLs(attempt,node,s,rs)
        t <- combineTypings(t1,t2)
      } yield t
//       case _        => errStr(s"Multiple inheritance not supported yet: ${es.map(_.show).mkString(",")}")
    }
  }

  private[validator] def checkShapeExtend(
      attempt: Attempt,
      node: RDFNode,
      shape: Shape,
      extendLabel: ShapeLabel
  ): CheckTyping =
    for {
      extendSe  <- getShape(extendLabel)
      _ <- { println(s"""|checkShapeExtend(
                         |  node=${node.show},
                         |  shape=${shape.show},
                         |  base=${extendLabel.show}
                         |""".stripMargin) ; ok(()) }
      neighs <- getNeighs(node)
      _ <- { println(s"Neighs of ${node.show} = ${neighs}") ; ok(()) }

      // TODO: Move the following code to Neighs.scala
      partitions = SetUtils.pSet(neighs.toList.toSet)
      _      <- checkSomeLazyList(
        partitions.map(checkPartitionPair(extendSe, shape, attempt, node)), 
        NoPartition(node,attempt,shape,extendLabel,neighs)
      )
      typing <- getTyping
    } yield typing

  private def checkPartitionPair(extended: ShapeExpr, 
                                 shape: Shape, 
                                 attempt: Attempt, 
                                 node: RDFNode)
                                 (pair: (Set[Arc], Set[Arc])
                                ): Check[ShapeTyping] = {
    val (neighs1, neighs2) = pair
    (for {
      _ <- info(s"""|------------------------
                    |checkPartitionPair/start
                    |neighs1=$neighs1
                    |neighs2=$neighs2
                    |Before checkNodeShapeExpr(${node.show},${extended.show})
                    |""".stripMargin)
      typing1 <- runLocal(
        checkNodeShapeExpr(attempt, node, extended), 
        _.addLocalNeighs(node, Neighs.fromSet(neighs1))
         .addVisited(shape.id) 
      )
      _ <- infoTyping(typing1, s"""|step1/checkPartitionPair: 
                                   |result of checkNodeShapeExpr(${node.show},${extended.show},${neighs1.toList.map(_.show).mkString(",")}""".stripMargin, schema.prefixMap)
      typing2 <- runLocal(checkNeighsShape(attempt, node, Neighs.fromSet(neighs2), shape), _.addLocalNeighs(node,Neighs.fromSet(neighs2)))
      _ <- infoTyping(typing2, s"""|step2/checkPartitionPair: 
                                   |result of checkNeighsShape(${node.show}, ${shape.show}, ${neighs2.toList.map(_.show).mkString(",")}})
                                   |""".stripMargin, schema.prefixMap)
      typing <- combineTypings(typing1,typing2)
      nodesPrefixMap <- getNodesPrefixMap
      _ <- infoTyping(typing, s"""|step3/result of checkPartitionPair
                                  |${typing.showShort(nodesPrefixMap, schema.prefixMap)}
                                  |""".stripMargin, schema.prefixMap)
    } yield typing /*(typing2, true)) orElse
      (for {
        t <- getTyping
      } yield (t, false)) */
    )  
  }

/*  private def noPartition(
     attempt: Attempt, 
     node: RDFNode, 
     s: Shape, 
     label: ShapeLabel,
     neighs: Neighs
     ): Check[(ShapeTyping, Boolean)] =
     err(NoPartition(node,attempt,s,label,neighs)) */

/*  private def checkNeighsShapeExpr(
      attempt: Attempt,
      node: RDFNode,
      neighs: Neighs,
      se: ShapeExpr
  ): Check[(ShapeTyping, Boolean)] =
    se match {
      case s: Shape =>
        (for {
          t <- checkNeighsShape(attempt, node, neighs, s)
          _ <- { println(s"Failed checkNeighsShape(node=${node.show}, neighs=$neighs, se=${se.show} passed with $t") ; ok(())}
        } yield (t, true)) orElse {
          for {
            _ <- { println(s"Failed checkNeighsShape(node=${node.show}, neighs=$neighs, se=${se.show} failed") ; ok(())}
            t <- getTyping
          } yield (t, false)
        }
      case sd: ShapeDecl => ???  
      case _ => {
        println(s"Not implemented yet extends with a non shape base: $se")
        errStr(s"Not implemented yet extends with a non shape base: $se")
      }
    } */

  private def checkNeighsShape(attempt: Attempt, node: RDFNode, neighs: Neighs, s: Shape): CheckTyping =
    if (s.hasRepeatedProperties(schema))
      checkNeighsShapeWithTable(attempt, node, neighs, s)
    else {
      // TODO
      checkNeighsShapeWithTable(attempt, node, neighs, s)
    }

  private def checkNeighsShapeWithTable(
      attempt: Attempt,
      node: RDFNode,
      neighs: Neighs,
      s: Shape
  ): CheckTyping = {
    for {
      tableRbe <- mkTable(s.expression, s.extra.getOrElse(List()), schema.prefixMap)
      (cTable, rbe) = tableRbe
      // _ <- info(s"cTable: $cTable")
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
      // _ <- info(s"Before checkCandidates:\n ${candidates.cs.map(_.show).mkString(",")}\nTable:${cTable.show}\n")
      typing <- checkCandidates(attempt, bagChecker, cTable)(candidates)
      // _ <- info(s"After checkCandidates: $typing")
      _ <- checkOptSemActs(attempt,node, s.actions)
    } yield {
      // println(s"End of checkShape(attempt=${attempt.show},node=${node.show},shape=${s.show})=${typing.show}")
      typing
    }
  }

  private def checkShapeBase(attempt: Attempt, node: RDFNode, s: Shape): CheckTyping = {
    info(s"checkShapeBase $node FlatShape? ${s.isFlatShape(schema)}") *> 
    (s match {
      case _ if s.isEmpty => addEvidence(attempt.nodeShape, s"Node $node matched empty shape")
      case _ if s.isFlatShape(schema) =>
        for {
          flatShape <- fromEitherString(s.flattenShape(schema))
          pm <- getNodesPrefixMap
          typing    <- ValidateFlatShape(this,pm,schema.prefixMap).checkFlatShape(attempt, node, flatShape)
        } yield typing
      case _ =>
        for {
          paths <- getPaths(s, schema)  
          neighs <- getNeighPaths(node, paths)
          typing <- checkNeighsShape(attempt, node, neighs, s)
        } yield typing
    })
}

  private def checkNoStrangeProperties(node: RDFNode, paths: List[Path], attempt: Attempt): Check[Unit] =
    for {
      s   <- getNotAllowedPredicates(node, paths)
      _ <- checkCond(s.isEmpty, attempt, ExtraPropertiesClosedShape(node,s.toList), "Closed properties with no extra property")
    } yield ()

  private def checkOptSemActs(attempt: Attempt, node: RDFNode, maybeActs: Option[List[SemAct]]): Check[Unit] =
    maybeActs match {
      case None     => ok(())
      case Some(as) => checkSemActs(attempt,node, as)
    }

  private def checkSemActs(attempt: Attempt, node: RDFNode, as: List[SemAct]): Check[Unit] =
    for {
      _ <- checkAll(as.map(checkSemAct(attempt,node, _)))
    } yield ()

  private def checkSemAct(attempt: Attempt, node: RDFNode, a: SemAct): Check[Unit] =
    for {
      rdf <- getRDF
      eitherResult   <- runAction(a.name, a.code, node, rdf)
      _ <- fromEither(eitherResult.leftMap(exc => SemanticActionException(attempt, node, a, exc)))
    } yield ()

  private def runAction(name: IRI, code: Option[String], node: RDFNode, rdf: RDFReader): Check[Either[Throwable,Unit]] = {
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


  private def checkRests(
      rests: NoCandidates,
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
    val ts: List[Either[String, Unit]] = rests.cs.map(checkRest(_, extras, isClosed, ignoredPathsClosed))
    val r: Either[String, Unit]        = ts.foldLeft(zero)(combine)
    r.fold(e => errStr(e), _ => ok(()))
  }

  private def checkRest(
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

  private def mkTable(maybeTe: Option[TripleExpr], extra: List[IRI], prefixMap: PrefixMap): Check[(CTable, Rbe_)] = {
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
  private def calculateCandidates(neighs: Neighs, table: CTable): Check[(Candidates, NoCandidates)] = {
    val candidates = table.neighs2Candidates(neighs.toList)
    val (cs, rs)   = candidates.cs.partition(matchable)
    // println(s"Candidates partitioned: cs:\n${cs.map(_.show).mkString(s"\n")}\nrs:${rs.map(_.show).mkString(s"\n")}\n")
    ok((Candidates(cs), NoCandidates(rs.map(_.arc))))
  }

  private def matchable(c: Candidate): Boolean = {
    c.crefs.nonEmpty
  }

  private[validator] def checkCandidates(attempt: Attempt, 
                     bagChecker: BagChecker_, 
                     table: CTable)
                     (candidates: Candidates): CheckTyping = {
    val as: List[CandidateLine] = 
      SeqUtils.transpose(
       candidates.cs.map(c => (c.arc, c.crefs))).map(CandidateLine(_))

    as.length match {
      case 1 => { // Deterministic
        checkCandidateLine(attempt, bagChecker, table)(as.head)
      }
      case 0 => {
        err(NoCandidateLine(attempt, table))
      }
      case n => {
        val checks: List[CheckTyping] =
          as.map(checkCandidateLine(attempt, bagChecker, table)(_))
        checkSome(checks, NoCandidate(attempt,bagChecker,as,table))
      }
    }
  }

  private def checkCandidateLine(attempt: Attempt, bagChecker: BagChecker_, table: CTable)(
      cl: CandidateLine
  ): CheckTyping = {
    val bag = cl.mkBag
    bagChecker
      .check(bag, false)
      .fold(
        e => err(ErrRBEMatch(attempt,cl,table,bag,bagChecker.rbe,e.head)),
        bag => {
          // println(s"Matches RBE...")
          val nodeConstraints = cl.nodeConstraints(table)
          val checkNodeConstraints: List[CheckTyping] =
            nodeConstraints.map {
              case (node, pair) => {
                val (shapeExpr, maybeSemActs) = pair
                for {
                  _ <- info(s"checkCandidateLine|checkNodeShapeExpr(${node.show},${shapeExpr.show})")
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


  def validateNodeStart(rdf: RDFReader, node: IRI): IO[Result] = {
    runValidator(checkNodeStart(node), rdf)
  }

  def validateNodeDecls(rdf: RDFReader): IO[Result] = {	
    runValidator(checkTargetNodeDeclarations, rdf)	
  }

  def validateNodeShape(rdf: RDFReader, node: IRI, shape: String): IO[Result] = {	
    ShapeLabel	
      .fromString(shape)	
      .fold(	
        e => IO.raiseError(StringError(s"Can not obtain label from $shape")),	
        label => runValidator(checkNodeShapeLabel(node, label), rdf)	
      )	
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

  def empty(builder: RDFBuilder): IO[Validator] = for {
    schema <- ResolvedSchema.empty
  } yield Validator(schema = schema, builder = builder)

  /**
    * Validate RDF according to a Shapes Schema
    *
    * @param schema: ShEx schema
    * @param fixedShapeMap: Shape map
    * @param rdf: RDF to validate
    * @param builder: RDF builder to return subgraph validated
    * @return Result of validation
    */
  def validate(schema: ResolvedSchema, 
               fixedShapeMap: FixedShapeMap, 
               rdf: RDFReader, 
               builder: RDFBuilder
              ): IO[Result] = {
    val validator = Validator(schema, NoAction, builder)
    validator.validateShapeMap(rdf, fixedShapeMap)
  } 

}
