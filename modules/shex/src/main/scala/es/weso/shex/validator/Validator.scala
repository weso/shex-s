package es.weso.shex.validator

import cats._
import implicits._
import cats.effect.IO
import es.weso.shex._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rbe.interval.IntervalChecker
import es.weso.rbe.Empty
import es.weso.utils.SetUtils
import es.weso.shex.implicits.showShEx._
import es.weso.shapemaps.{BNodeLabel => BNodeMapLabel, IRILabel => IRIMapLabel, Start => StartMapLabel, _}
import es.weso.shex.actions.TestSemanticAction
import Function.tupled
import es.weso.shex.validator.ShExError._
import es.weso.shex.validator.ConstraintRef.{showConstraintRef => _}
import es.weso.utils.internal.CollectionCompat._
import es.weso.utils.VerboseLevel

/**
  * ShEx validator
  */
case class Validator(schema: ResolvedSchema,
                     externalResolver: ExternalResolver = ExternalResolver.NoAction,
                     builder: RDFBuilder
                     )
    extends ShExChecker
    with ShowValidator {

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

  private def checkShapeMap(shapeMap: FixedShapeMap): CheckTyping = 
   for {
    // _ <- info(s"checkShapeMap")
    r <- checkNodesShapes(shapeMap)
    // _ <- info(s"end of checkShapeMap")
  } yield r

  private def checkNodesShapes(fixedMap: FixedShapeMap): CheckTyping =
    for {
      ts <- checkAll(fixedMap.shapeMap.toList.map(tupled(checkNodeShapesMap)))
      t  <- combineTypings(ts)
      nodesPrefixMap <- getNodesPrefixMap
      _ <- debug(s"end of checkNodeShapes: ${t.showShort(nodesPrefixMap,schema.prefixMap)}")
    } yield t 

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
    else 
      getRDF
      .flatMap(rdf => err(ShExError.NotConformantConforms(node, label, rdf)))
    

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
    schema.getShape(label).fold(
      e => err(ShExError.LabelNotFound(label,e,schema.labels)),
      ok(_)
    ) 

  private def checkNodeShapeLabel(node: RDFNode, shape: ShapeLabel): CheckTyping = {
    cond(verifyShapeLabel(shape),
      (shapeLabel: ShapeLabel) => checkNodeLabel(node, shapeLabel),
      err => for {
          t <- getTyping
        } yield t.addNotEvidence(node, ShapeType(ShapeExpr.fail, Some(shape), schema), err)
    )
  }

  private def checkNodeStart(node: RDFNode): CheckTyping = getRDF.flatMap { rdf => 
    schema.start match {
      case None => err(NoStart(node,rdf))
      case Some(shape) => {
        val shapeType = ShapeType(shape, Some(Start), schema)
        val attempt   = Attempt(NodeShape(node, shapeType), None, rdf)
        runLocalSafeTyping(checkNodeShapeExpr(attempt, node, shape), 
          _.addType(node, shapeType), (err, t) => {
          t.addNotEvidence(node, shapeType, err)
        })
      }
    }
  }

  private def verifyShapeLabel(label: ShapeLabel): Check[ShapeLabel] = {
    if (schema.labels contains label) ok(label)
    else err(LabelNotFound(label, "", schema.labels))
  }

  private def checkNodeLabelSafe(node: RDFNode, label: ShapeLabel, shape: ShapeExpr): CheckTyping = {
    val shapeType = ShapeType(shape, Some(label), schema)
    
    for {
      rdf <- getRDF
      attempt = Attempt(NodeShape(node, shapeType), None,rdf)
      t <- runLocalSafeTyping(
          bind(
            checkOptSemActs(attempt,node, schema.startActs),
            checkNodeShapeExpr(attempt, node, shape)
          ),
          t => { 
            debug(s"checkNodeLabelSafe: adding type ${node.show}@+${shapeType.label.map(_.toRDFNode.show).getOrElse("?")}")
            t.addType(node, shapeType)
          },
          (err, t) => {
            debug(s"checkNodeLabelSafe: Error, addNotEvidence ${node.show}@-${shapeType.label.map(_.toRDFNode.show).getOrElse("?")}")
            t.addNotEvidence(node, shapeType, err)
          }
        )
      _ <- infoTyping(t, "end of checkNodeLabelSafe:", schema.prefixMap)  
    } yield t
  }

  private def addNot(node: RDFNode, label: ShapeLabel, typing: ShapeTyping)(err: ShExError): CheckTyping = {
    val shapeType = ShapeType(ShapeExpr.fail, Some(label), schema)
    ok(typing.addNotEvidence(node, shapeType, err))
  }


  private[validator] def checkNodeLabel(node: RDFNode, label: ShapeLabel): CheckTyping = {
    for {
      _ <- debug(s"checkNodeLabel(${node.show},${label.show})")
      // strInheritance <- fromIO(schema.inheritanceGraph.show(label => schema.prefixMap.qualify(label.toRDFNode)))
      // _ <- debug(s"InheritanceGraph\n${strInheritance}\n")
      typing <- getTyping
      visited <- getVisited
      _ <- debug(s"Visited: ${visited.map(_.show).mkString(",")}")
      newTyping <- if (typing.hasInfoAbout(node, label) && !(visited contains label) // visited contains the labels that have been visited by extends and should be ignored
      ) {
        debug(s"checkNodeLabel(${node.show},${label.show}). Typing already contains label ${label.show}. Visited: ${visited}") >>
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

  private def checkDescendants(
    node: RDFNode,
    s: ShapeExpr, 
    attempt: Attempt): Check[(ShapeTyping, Option[ShapeLabel])] = for {
     descendants <- fromIO(s.id.map(schema.inheritanceGraph.ancestors(_)).getOrElse(IO(Set[ShapeLabel]())))
     visited <- getVisited
     filteredDescendants = descendants.diff(visited)
/*     _ <- debug(
       s"""|checkDescendants(${showSE(s)=${sh(descendants)}
           |visited=${sh(visited)}
           |descendants filtered=${sh(filteredDescendants)}""".stripMargin) */

     result <- if (filteredDescendants.isEmpty) for {
         _ <- debug(s"checkDescentants(${node.show}@${showSE(s)}): Empty descendants")
         t <- getTyping
       } yield (t,none[ShapeLabel])
      else 
       debug(s"checkDescentants(${node.show}@${showSE(s)}): ${filteredDescendants.toList.map(_.show).mkString(",")}") *>
       checkSomeFlagValue(filteredDescendants.toLazyList,
         (d: ShapeLabel) => for {
           se <- getShape(d)
           st = ShapeType(se,se.id,schema)
           t <-
             runLocalSafeTyping(
               runLocal(checkNodeShapeExpr(attempt,node,se), _.addVisited(se.id)),
               _.addType(node,st),
               (err,tt) => tt.addNotEvidence(node,st,err)
             )
           _ <- if (t.getOkValues(node) contains ShapeType(se,Some(d),schema)) ok(())
                else 
                  debug(s"Descendant ${d.toRDFNode.show} failed on node ${node.show}") *>
                  errStr[Unit](s"Descendant ${d.toRDFNode.show} failed on node ${node.show}") 
         } yield t, getTyping)
  } yield result

  private def checkNodeShapeExpr(
     attempt: Attempt, 
     node: RDFNode, 
     s: ShapeExpr): CheckTyping = {
   for {
    pair <- checkDescendants(node, s, attempt)
    (t,b) = pair
    newT <- b match {
              case Some(label) => infoTyping(t,s"Descendant passed (${label.toRDFNode.show}) with typing ",schema.prefixMap) *>
               ok(t.addEvidence(node,ShapeType(s,s.id,schema),s"Descendant ${label.toRDFNode.show} of shape passed it"))
              case None => {
               infoTyping(t, s"checkNodeShapeExpr(${node.show},${showSE(s)}, no descendants passed. calling checkNodeShapeExprNoDescendants...", schema.prefixMap) *>
               runLocalTyping(
                 checkNodeShapeExprNoDescendants(attempt,node,s),
                 _.combine(t))
             }
            }
    _ <- infoTyping(newT,s"Result of checkNodeShapeExpr($node,${showSE(s)} = ", schema.prefixMap)             
   } yield newT
  }

  private def checkNodeShapeExprNoDescendants(attempt: Attempt, node: RDFNode, s: ShapeExpr): CheckTyping = {
   for {
     nodesPrefixMap <- getNodesPrefixMap
     _ <- debug(s"checkNodeShapeExprNoDescendants(${node.show},${showSE(s)})")
     typing <- s match {
       case so: ShapeOr => checkOr(attempt, node, so.shapeExprs)
       case sa: ShapeAnd => 
         debug(s"shapeAnd") *>
         checkAnd(attempt, node, sa.shapeExprs)
       case sn: ShapeNot => checkNot(attempt, node, sn.shapeExpr)
       case nc: NodeConstraint => checkNodeConstraint(attempt, node, nc)
       case s: Shape => for {        
         t <- checkShape(attempt, node, s)
         _ <- infoTyping(t,s"Result of checkShape: ",schema.prefixMap)
       } yield t
       case sr: ShapeRef => checkRef(attempt, node, sr.reference)
       case se: ShapeExternal => checkExternal(attempt, node, se)
       case sd: ShapeDecl => 
         debug(s"checkShapeDecl(${node.show}, ${showSE(sd)})") *>
         checkShapeDecl(attempt,node, sd)
     }
     _ <- infoTyping(typing, s"end of checkNodeShapeExpr(${node.show},${showSE(s)})", schema.prefixMap)
   } yield typing
  }

  private def checkAnd(attempt: Attempt, node: RDFNode, ses: List[ShapeExpr]): CheckTyping =
    for {
      _ <- debug(s"checkAnd($node, ${ses.map(showSE(_)).mkString(",")}")
      ts <- checkAll(ses.map(se => checkNodeShapeExpr(attempt, node, se)))
      t  <- combineTypings(ts)
    } yield t

  private def checkOr(attempt: Attempt, node: RDFNode, ses: List[ShapeExpr]): CheckTyping = {
    val vs = ses.map(se => checkNodeShapeExpr(attempt, node, se))
    for {
      t1 <- checkSome(
        vs,
        StringError(
          s"None of the alternatives of OR(${ses.map(showSE(_)).mkString(",")}) is valid for node ${node.show}"
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
          s"${node.show} does not satisfy ${showSE(s)}. Negation declared in ${parentShape.show}. Error: $e"
        )
        t2 <- addEvidence(attempt.nodeShape, s"${node.show} satisfies not(${showSE(s)})")
        t  <- combineTypings(List(t1, t2))
      } yield t
    val handleNotError: ShapeTyping => Check[ShapeTyping] = t =>
      errStr(s"Failed NOT(${showSE(s)}) because node ${node.show} satisfies it")
    cond(check, handleNotError, handleError)
  }


  private def checkShapeDecl(attempt: Attempt, node: RDFNode, sd: ShapeDecl): CheckTyping = for {
    t <- checkNodeShapeExpr(attempt,node,sd.shapeExpr)
    _ <- infoTyping(t,"checkShapeDecl. Result of checkNodeShapeExpr: ",schema.prefixMap)
    descendants <- getDescendants(sd)
    _ <- debug(s"""|checkShapeDecl: Descendants of ${showSE(sd)}=${descendants.map(d => schema.prefixMap.qualify(d.toRDFNode)).mkString(",")}
                  |""".stripMargin)
    rdf <- getRDF
    newT <- if (sd._abstract) 
      cond(checkSome(descendants.toList.map(checkHasType(node, t, attempt)), 
                           AbstractShapeErr(node,sd,rdf)),
            (_: Unit) => ok(t),
            _ => {
              debug(s"No descendants of ${showSE(sd)} found that match ${node} removing ${showSE(sd)} shape") >>
              removeShapeType(node,sd,t)
            }) // orElse (ok(t.addNotEvidence(node,ShapeType(sd,sd.id,schema), AbstractShapeErr(node,sd))))
    else ok(t)
  } yield newT

  private def removeShapeType(node: RDFNode, s: ShapeExpr, t: ShapeTyping): CheckTyping = 
   getRDF.flatMap(rdf => 
   ok(t.addNotEvidence(node,ShapeType(s,s.id,schema), AbstractShapeErr(node,s,rdf))))
    

  private def getDescendants(s: ShapeExpr): Check[Set[ShapeLabel]] = s.id match {
    case None => ok(Set())
    case Some(lbl) => for {
      ds <- fromIO(schema.inheritanceGraph.ancestors(lbl))
    } yield ds 
  }
   

  private def checkHasType(node: RDFNode, typing: ShapeTyping, attempt: Attempt)(shapeLabel: ShapeLabel): Check[Unit] =
    if (typing.hasNoType(node, shapeLabel)) {
      getRDF.flatMap(rdf => 
      err(HasNoType(node, shapeLabel, typing, attempt, rdf)))
    } else ok(())

  private def checkRef(attempt: Attempt, node: RDFNode, ref: ShapeLabel): CheckTyping =
    for {
      t <- checkNodeLabel(node, ref)
      _ <- checkHasType(node, t, attempt)(ref)
    } yield t

  private def checkNodeConstraint(attempt: Attempt, node: RDFNode, s: NodeConstraint): CheckTyping =
    for {
      t1 <- optCheck(s.nodeKind, checkNodeKind(attempt, node), getTyping)
      t2 <- optCheck(s.values, checkValues(attempt, node), getTyping)
      t3 <- optCheck(s.datatype, checkDatatype(attempt, node), getTyping)
      t4 <- checkXsFacets(attempt, node)(s.xsFacets)
      t  <- combineTypings(List(t1, t2, t3, t4))
    } yield {
      t
    }

  private  def getExternalShape(se: ShapeExternal): Check[ShapeExpr] = se.id match {
    case None => err(NoLabelExternal(se))
    case Some(label) => fromIO(externalResolver.getShapeExpr(label, se.annotations))
  }


  private def checkExternal(attempt: Attempt,
                                       node: RDFNode,
                                       se: ShapeExternal): CheckTyping = {
    for {
      externalShape <- getExternalShape(se)
      rdf <- getRDF
      newAttempt = Attempt(NodeShape(node, ShapeType(externalShape, se.id, schema)), attempt.path, rdf)
      t <- checkNodeShapeExpr(newAttempt, node, externalShape)
    } yield t
  }

  private def checkValueSetValue(attempt: Attempt, node: RDFNode)(v: ValueSetValue): CheckTyping = {
   val r: CheckTyping = ValueChecker(schema).checkValue(attempt, node, v)
   r
  }

  private def checkValues(attempt: Attempt,
                                     node: RDFNode)
                                    (values: List[ValueSetValue]): CheckTyping = {
    val cs: List[CheckTyping] = values.map(checkValueSetValue(attempt,node))
    checkSome(cs, StringError(s"${node.show} does not belong to [${values.map(_.show).mkString(",")}]"))
  }

  private def checkDatatype(attempt: Attempt, node: RDFNode)(datatype: IRI): CheckTyping =
    for {
      rdf   <- getRDF
      hasDatatype <- fromIO(rdf.checkDatatype(node, datatype))
      check <- checkCond(
        hasDatatype,
        attempt,
        CheckDatatypeError(node, datatype,rdf),
        s"${node.show} has datatype ${datatype.show}")
    } yield check

  private def checkXsFacets(attempt: Attempt, node: RDFNode)(xsFacets: List[XsFacet]): CheckTyping = {
    if (xsFacets.isEmpty) getTyping
    else
      for {
        rdf <- getRDF
        t   <- FacetChecker(schema, rdf).checkFacets(attempt, node)(xsFacets)
      } yield t
  }

  private def checkNodeKind(attempt: Attempt, node: RDFNode)(nk: NodeKind): CheckTyping = {
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

  private def checkShape(attempt: Attempt, node: RDFNode, s: Shape): CheckTyping =
    s._extends match {
      case None     => checkShapeRestricts(attempt, node, s)
      case Some(es) => 
        debug(s"checkShape(${node}@${showSE(s)}) with extends ${es.map(_.show).mkString(",")}") *>
        checkShapeExtendLs(attempt, node, s, es)
    }

  private[validator] def checkShapeExtendLs(
      attempt: Attempt,
      node: RDFNode,
      s: Shape,
      es: List[ShapeLabel]
  ): CheckTyping = {
    val msg = s"checkShapeExtendLs(${node.show}@${showSE(s)} extendList ${es.map(_.show).mkString(",")}"
    debug(msg) *> 
    getNeighs(node).flatMap(neighs => {
     val partitions = SetUtils.partition(neighs.toList.toSet, es.length + 1) 
     val noPartition: Check[ShapeTyping] = err[ShapeTyping](NoPartition(node,attempt,s,es,neighs))
     checkSomeFlagValue(partitions, checkPartition(attempt,node,s,es), noPartition).flatMap{ case (t,b) => {
     info(b match {
        case None => s"$msg| No partition passed"
        case Some(ps) => s"$msg| Passed with partition ${ps.map(_.show).mkString(",")}"
      })  *>
     infoTyping(t, s"end of $msg. t = ", schema.prefixMap) *>
     ok(t)       
     }}})  
    }
/*    es match {
      case Nil      => checkShapeRestricts(attempt, node, s)
      case e :: Nil => for {
        _ <- info(s"checkShapeExtend with 1 extend ${e.toRDFNode.show}")
        t <- checkShapeExtend(attempt, node, s, e)
        _ <- infoTyping(t,"After checkShapeExtend",schema.prefixMap)
      } yield t 
      case e :: rs  => for {
        _ <- debug(s"1. checkShapeExtendLs(${node.show}@${showSE(s)} with LS = ${es.map(_.toRDFNode.show).mkString(",")})")
        t1 <- checkShapeExtend(attempt,node,s,e)
        _ <- infoTyping(t1,s"2. checkShapeExtendLs(${es.map(_.toRDFNode.show).mkString(",")}): After checkShapeExtend, t1 = ",schema.prefixMap)
        t2 <- runLocalTyping(checkShapeExtendLs(attempt,node,s,rs), _.combine(t1))
        _ <- infoTyping(t2,s"3. checkShapeExtendLs(${es.map(_.toRDFNode.show).mkString(",")}): After checkShapeExtend, t2 = ",schema.prefixMap)
        // t <- combineTypings(t1,t2)
      } yield t2
    } */
  

  private def checkPartition(attempt: Attempt, node: RDFNode, base: Shape, es: List[ShapeLabel])(partition: List[Set[Arc]]): Check[ShapeTyping] = {
    val baseNeighs = partition.head
    val pairs = es.zip(partition.tail)
    val neighs = Neighs.fromSet(baseNeighs)
    getRDF.flatMap(rdf => 
    checkPartitionsExtend(attempt, node, pairs).flatMap(t => 
    runLocalTyping(
      runLocal(checkNeighsShape(attempt, node, neighs, base), 
               _.addLocalNeighs(node, neighs)
               ), _ => t
    )))
  }
  
  private def checkPartitionsExtend(attempt: Attempt, node: RDFNode, pairs: List[(ShapeLabel, Set[Arc])]): Check[ShapeTyping] = {
    checkAll(
      pairs
      .map{ 
        case (lbl, neighs) => checkPartitionExtend(attempt,node, lbl, neighs) 
    })
    .flatMap(ts => combineTypings(ts))
  }

  private def checkPartitionExtend(attempt: Attempt, node: RDFNode, lbl: ShapeLabel, neighs: Set[Arc]): Check[ShapeTyping] = {
    getRDF.flatMap(rdf => 
    getShape(lbl).flatMap(shapeExpr => {
     val st = ShapeType(shapeExpr,Some(lbl),schema)
     runLocalTyping(
        runLocal(checkNodeShapeExprNoDescendants(attempt,node, shapeExpr),
        _.addLocalNeighs(node, Neighs.fromSet(neighs))
         .addVisited(Some(lbl))),
         _.addType(node,st)
      )
    }
   ))
  }


/*
  private def checkShapeExtend(
      attempt: Attempt,
      node: RDFNode,
      shape: Shape,
      extendLabel: ShapeLabel
  ): CheckTyping = {

    def noPartition(neighs:Neighs, extendSe: ShapeExpr): Check[ShapeTyping] = 
      err[ShapeTyping](NoPartition(node,attempt,shape,List(extendLabel),neighs))

    for {
      extendSe  <- getShape(extendLabel)
      nodesPrefixMap <- getNodesPrefixMap
      _ <- { debug(s"""|checkShapeExtend(
                      |  node=${node.show},
                      |  shape=${shape.show},
                      |  base=${extendLabel.show}
                      |  attempt=${attempt.showQualified(nodesPrefixMap,schema.prefixMap)}
                      |""".stripMargin) ; ok(()) }
      _ <- showCurrentTyping("checkShapeExtend: current typing: ", schema.prefixMap)                   
      neighs <- getNeighs(node)
      _ <- { debug(s"Neighs of ${node.show} = ${neighs}") ; ok(()) }
      partitions = SetUtils.pSet(neighs.toList.toSet)
      pair      <- checkSomeFlagValue[(Set[Arc],Set[Arc]),ShapeTyping](
        ls = partitions,
        check = checkPartitionPair(extendSe, extendLabel, shape, attempt, node), 
        last = noPartition(neighs,extendSe)
      )
      (t,b) = pair
      _ <- b match {
        case Some(ns) => debug(s"Passed with neighs: ${ns}")
        case None => debug(s"No partition passed")
      }
      _ <- infoTyping(t,s"<<<Typing after checkShapeExtend(${node.show}@${showSE(shape)}) (b=${b})",schema.prefixMap)
    } yield t
  }

  private def errPartitionFailed(node: RDFNode, attempt: Attempt, shape: Shape, extendLabel: ShapeLabel, pair: (Set[Arc], Set[Arc])): Check[ShapeTyping] = 
    err(PartitionFailed(node,attempt,shape,extendLabel,pair))

  private def checkPartitionPair(extended: ShapeExpr, 
                                 extendLabel: ShapeLabel,
                                 shape: Shape, 
                                 attempt: Attempt, 
                                 node: RDFNode)
                                 (pair: (Set[Arc], Set[Arc])
                                ): Check[ShapeTyping] = {
    val (neighs1, neighs2) = pair
    for {
      _ <- debug(s"""|------------------------
                    |checkPartitionPair: ${node.show}@${showSE(shape)} extends: ${extendLabel.toRDFNode.show}|
                    |neighs1=$neighs1
                    |neighs2=$neighs2
                    |Before checkNodeShapeExpr(${node.show},${showSE(extended)})
                    |""".stripMargin)
      label = shape.id match {
        case None => attempt.nodeShape.shape.label
        case x@Some(_) => x
      }
      _ <- debug(s"VisitedLabel: ${label.show}")
      rdf <- getRDF
      st = ShapeType(extended,Some(extendLabel),schema)
      typing1 <- runLocalSafeTyping(
        runLocal(checkNodeShapeExprNoDescendants(attempt,node, extended),
        _.addLocalNeighs(node, Neighs.fromSet(neighs1))
         .addVisited(label)),
         _.addType(node,st),
        (e,t)  => 
          t.addNotEvidence(node,st,ExtendFails(node,extendLabel,attempt,e,rdf))
      )
      pair <- if (typing1.getOkValues(node) contains st) for {
       _ <- infoTyping(typing1, s"""| step1/checkPartitionPair(${node.show}@${showSE(shape)} extendLabel: ${extendLabel.toRDFNode.show}) / typing1 = """.stripMargin, schema.prefixMap)
       typing2 <- runLocalSafeTyping(runLocal(
         checkNeighsShape(attempt, node, Neighs.fromSet(neighs2), shape), 
         _.addLocalNeighs(node,Neighs.fromSet(neighs2))
         ), _ => typing1,
         (e,t)  => 
          t.addNotEvidence(node,ShapeType(shape, shape.id, schema),BaseFails(node,shape,attempt,e,rdf))
         )
//       _ <- infoTyping(typing2, s"""| step2/checkPartitionPair(${node.show}@${showSE(shape)} extendLabel: ${extendLabel.toRDFNode.show}) / typing2= """.stripMargin, schema.prefixMap)
       typing = typing2 // <- combineTypings(typing1,typing2)
       _ <- infoTyping(typing, s"""| step2/checkPartitionPair(${node.show}@${showSE(shape)} extendLabel: ${extendLabel.toRDFNode.show}) / typing  = """.stripMargin, schema.prefixMap)
       } yield typing
      else errPartitionFailed(node,attempt,shape,extendLabel,pair) 
    } yield pair 
  }
*/
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
      _ <- debug(s"""|checkNeighsShapeWithTable: ${node.show}
                     |neighs=${neighs}
                     |shape=${showSE(s)}""".stripMargin)
      tableRbe <- mkTable(s.expression, s.extra.getOrElse(List()), schema.prefixMap)
      (cTable, rbe) = tableRbe
      // _ <- info(s"cTable: $cTable")
      bagChecker    = IntervalChecker(rbe)
      csRest <- calculateCandidates(neighs, cTable)
      (candidates, rest) = csRest
      _     <- checkRests(rest, s.extraPaths, s.isClosed, ignoredPathsClosed, s, attempt)
      paths <- fromEither(s.paths(schema).leftMap(StringError(_)))
      // _ <- debug(s"Checking closed condition with paths=${paths}, neighs=${neighs}")
      _ <- {
        if (s.isClosed) {
          checkNoStrangeProperties(node, paths.toList, attempt, neighs)
        } else ok(())
      }
      // _ <- info(s"Before checkCandidates:\n ${candidates.cs.map(_.show).mkString(",")}\nTable:${cTable.show}\n")
      typing <- checkCandidates(attempt, bagChecker, cTable, node)(candidates)
      _ <- info(s"checkNeighsShapeWithTable: after checkCandidates: $typing")
      _ <- checkOptSemActs(attempt,node, s.actions)
    } yield {
      // println(s"End of checkShape(attempt=${attempt.show},node=${node.show},shape=${s.show})=${typing.show}")
      typing
    }
  }

  private def checkShapeRestricts(attempt: Attempt, node: RDFNode, s: Shape): CheckTyping = 
   s.restricts match {
      case None     => checkShapeBase(attempt, node, s)
      case Some(rs) => 
        checkShapeRestrictLs(attempt, node, s, rs)
   }

  private def checkShapeRestrictLs(attempt: Attempt, node: RDFNode, s: Shape, rs: List[ShapeLabel]): CheckTyping = 
  rs match {
      case Nil      => checkShapeBase(attempt, node, s)
      case r :: Nil => for {
        t <- checkShapeRestrict(attempt, node, s, r)
        // _ <- infoTyping(t,"After checkShapeRestrict",schema.prefixMap)
      } yield t 
      case e :: rs  => err(MultipleRestricts(node,attempt,s,rs))
  }

  private def checkShapeRestrict(attempt: Attempt, node: RDFNode, s: Shape, rl: ShapeLabel): CheckTyping = 
  for {
    t1 <- checkNodeShapeLabel(node,rl)
    t2 <- checkShapeBase(attempt,node,s)
    t <- combineTypings(t1,t2)
  } yield t

  /* Check a shape without extends and restricts */
  private def checkShapeBase(attempt: Attempt, node: RDFNode, s: Shape): CheckTyping = {
    debug(s"checkShapeBase(${node.show}@{${showSE(s)}}, flatShape? ${s.isFlatShape(schema)}") *> 
    (s match {
      case _ if s.isEmpty => addEvidence(attempt.nodeShape, s"Node ${node.show} conforms empty shape")
      case _ if s.isFlatShape(schema) =>
        for {
          flatShape <- fromEitherString(s.flattenShape(schema))
          pm <- getNodesPrefixMap
          typing    <- ValidateFlatShape(this,pm,schema.prefixMap,builder).checkFlatShape(attempt, node, flatShape)
        } yield typing
      case _ =>
        for {
          paths <- getPaths(s, schema)  
          neighs <- getNeighPaths(node, paths)
          typing <- checkNeighsShape(attempt, node, neighs, s)
        } yield typing
    })
}

  private def checkNoStrangeProperties(node: RDFNode, paths: List[Path], attempt: Attempt, neighs: Neighs): Check[Unit] =
    for {
      s   <- getNotAllowedPredicates(node, paths, neighs: Neighs)
      _ <- debug(s"NotAllowedPredicates: ${s}")
      _ <- checkCond(s.isEmpty, attempt, 
            ExtraPropertiesClosedShape(node,s.toList), "Closed properties with no extra property")
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

  private def runAction(name: IRI,
                        code: Option[String],
                        node: RDFNode,
                        rdf: RDFReader
                       ): Check[Either[Throwable,Unit]] = {
    val unit: Either[Throwable,Unit] = Right(())
    // println(s"Semantic action: $name/$code")
    for {
      r <- name match {
        case TestSemanticAction.`iri` => {
          fromIO(
            TestSemanticAction.runAction(code.getOrElse(""), node, rdf)
              .attempt
          )
        }
        case _ => {
          addAction2Log(Action(name, code))
          ok(unit)
        }
      }
    } yield r
  }


  private def checkRests(
      rests: NoCandidates,
      extras: List[Path],
      isClosed: Boolean,
      ignoredPathsClosed: List[Path],
      shape: Shape,
      attempt: Attempt
  ): Check[Unit] = {
    val zero: Either[ShExError, Unit] = ().asRight
    def combine(
      step: Either[ShExError, Unit], 
      current: Either[ShExError, Unit]
      ): Either[ShExError, Unit] =
      (step, current) match {
        case (Left(e1), _)        => Left(e1)
        case (_, Left(e2))        => Left(e2)
        case (Right(()), Right(())) => Right(())
      }
    val ts: List[Either[ShExError, Unit]] = rests.cs.map(checkRest(_, extras, isClosed, ignoredPathsClosed, shape, attempt))
    val r: Either[ShExError, Unit]        = ts.foldLeft(zero)(combine)
    r.fold(
      err(_), 
      _ => ok(())
      )
  }

  private def checkRest(
      rest: Arc,
      extras: List[Path],
      isClosed: Boolean,
      ignoredPathsClosed: List[Path],
      shape: Shape,
      attempt: Attempt
  ): Either[ShExError, Unit] = {
    val restPath = rest.path
    // Ignore extra predicates if they are inverse
    if (isClosed && restPath.isDirect) {
      // TODO: Review if the extra.contains(restpath) check is necessary
      // Extra has been implemented as a negation
      if (ignoredPathsClosed.contains(restPath) || extras.contains(restPath)) {
        ().asRight
      } else {
        ClosedShapeWithRests(shape,rest,attempt,ignoredPathsClosed,extras).asLeft
      }
    } else ().asRight
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
                     table: CTable,
                     node: RDFNode)
                     (candidates: Candidates): CheckTyping = {

    val as = candidates.getCandidateLines()                      
    as.length match {
      case 1 => { // Deterministic
        checkCandidateLine(attempt, bagChecker, table, node)(as.head)
      }
      case 0 => {
        getRDF.flatMap(rdf => err(NoCandidateLine(attempt, table, node, rdf)))
      }
      case n => {
        val checks: List[CheckTyping] =
          as.map(checkCandidateLine(attempt, bagChecker, table,node)(_))
        getRDF.flatMap(rdf =>   
        checkSome(checks, NoCandidate(attempt,bagChecker,as,table,node, rdf)))
      }
    }
  }

  private def checkCandidateLine(attempt: Attempt, bagChecker: BagChecker_, table: CTable, node: RDFNode)(
      cl: CandidateLine
  ): CheckTyping = {
    val bag = cl.mkBag
    bagChecker
      .check(bag, false)
      .fold(
        e => getRDF.flatMap(rdf => 
          err(ErrRBEMatch(attempt,cl,table,bag,bagChecker.rbe,e.head,node,rdf))),
        bag => {
          val nodeConstraints = cl.nodeConstraints(table)
          val checkNodeConstraints: List[CheckTyping] =
            nodeConstraints.map {
              case (node, pair) => {
                val (shapeExpr, maybeSemActs) = pair
                for {
                  _ <- debug(s"checkCandidateLine|checkNodeShapeExpr(${node.show},${showSE(shapeExpr)})")
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


  // Public methods 

  /**
   * Validate a node against the START declaration
   **/
  def validateNodeStart(rdf: RDFReader, node: IRI, verbose: VerboseLevel): IO[Result] = {
    runValidator(checkNodeStart(node), rdf, verbose)
  }

  /**
   * Validate a node following target declarations.
   * This methods follows SHACL convention and could be deprecated in the future
   * 
   **/
  def validateNodeDecls(rdf: RDFReader, verbose: VerboseLevel): IO[Result] = {	
    runValidator(checkTargetNodeDeclarations, rdf, verbose)	
  }

  /**
   * Validate a node against a shape
   **/
  def validateNodeShape(rdf: RDFReader, node: IRI, shape: String, verbose: VerboseLevel): IO[Result] = {	
    ShapeLabel	
      .fromString(shape)	
      .fold(	
        e => IO.raiseError(StringError(s"Can not obtain label from $shape")),	
        label => runValidator(checkNodeShapeLabel(node, label), rdf, verbose)	
      )	
  }

  /**
   * Validate a node against a shape map
   **/
  def validateShapeMap(rdf: RDFReader, 
                       shapeMap: FixedShapeMap, 
                       verbose: VerboseLevel): IO[Result] = 
  for {
    r <- runValidator(checkShapeMap(shapeMap), rdf, verbose)
  } yield r

  /**
   * Execute the validator with a given checker
   * param chk Checker
   * param rdf RDFReader
   * verbose boolean flag to show internal messages
   **/
  def runValidator(
   chk: Check[ShapeTyping], 
   rdf: RDFReader, 
   verbose: VerboseLevel): IO[Result] = for {
    r <- runCheck(chk, rdf, verbose)
    pm <- rdf.getPrefixMap
  } yield cnvResult(r, rdf, pm)

  private def cnvResult(r: CheckResult[ShExError, ShapeTyping, Log], rdf: RDFReader, rdfPrefixMap: PrefixMap): Result = 
   Result (
    for {
      shapeTyping <- r.toEither
      result      <- shapeTyping.toShapeMap(rdfPrefixMap, schema.prefixMap).leftMap(StringError.apply)
    } yield {
      (r.log, result)
    }
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
               builder: RDFBuilder,
               verbose: VerboseLevel
              ): IO[Result] = {
    val validator = Validator(schema, ExternalResolver.NoAction, builder)
    validator.validateShapeMap(rdf, fixedShapeMap, verbose)
  } 

}
