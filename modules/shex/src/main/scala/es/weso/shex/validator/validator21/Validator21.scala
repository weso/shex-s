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
import es.weso.shapemaps.{
  BNodeLabel => BNodeMapLabel,
  IRILabel => IRIMapLabel,
  Start => StartMapLabel,
  _
}
import es.weso.shex.actions.TestSemanticAction
import Function.tupled
import es.weso.shex.validator.ShExError._
import es.weso.shex.validator.ConstraintRef.{showConstraintRef => _}
import es.weso.utils.internal.CollectionCompat._
import es.weso.utils.VerboseLevel
import PartitionUtils._
import es.weso.shapemaps.Status._

/** ShEx validator for ShEx 2.1 (no extends, abstract)
  */
case class Validator21(
    schema: ResolvedSchema,
    externalResolver: ExternalResolver = ExternalResolver.NoAction,
    builder: RDFBuilder
) extends Validator
    with ShExChecker
    with ShowValidator
    with AllPaths {

  type ShapeChecker = ShapeExpr => CheckTyping
  type NodeShapeChecker = (RDFNode, Shape) => CheckTyping
  type NodeChecker = Attempt => RDFNode => CheckTyping

  val nodeValidator = NodeConstraintValidator(schema)

  private def checkTargetNodeDeclarations: CheckTyping =
    for {
      rdf <- getRDF
      nodeLabels <- getTargetNodeDeclarations(rdf)
      ts <- checkAll(nodeLabels.map { case (node, label) =>
        checkNodeLabel(node, label, None, Visited.empty, WithDescendants.followDescendants)
      })
      t <- combineTypings(ts)
    } yield t

  private def checkShapeMap(fixedMap: FixedShapeMap): CheckTyping =
    checkAll(fixedMap.shapeMap.toList.map(tupled(checkNodeShapesMap))).flatMap(ts =>
      combineTypings(ts)
    )

  private def checkNodeShapeMapLabel(node: RDFNode, label: ShapeMapLabel, info: Info): CheckTyping =
    info.status match {
      case Conformant =>
        label match {
          case StartMapLabel => checkNodeStart(node)
          case IRIMapLabel(_) | BNodeMapLabel(_) =>
            checkNodeShapeLabel(node, mkLabel(label))
        }
      case NonConformant =>
        label match {
          case StartMapLabel =>
            checkNodeStart(node).flatMap(c => checkNotConformant(node, Start, c))
          case IRIMapLabel(_) | BNodeMapLabel(_) =>
            val lbl = mkLabel(label)
            checkNodeShapeLabel(node, lbl).flatMap(c => checkNotConformant(node, lbl, c))
        }
      case _ =>
        errStr(s"Cannot check $node against status: ${info.status}")
    }

  private def checkNotConformant(node: RDFNode, label: ShapeLabel, c: ShapeTyping): CheckTyping =
    if (c.hasNoType(node, label)) ok(c)
    else
      getRDF.flatMap(rdf => err(ShExError.NotConformantConforms(node, label, rdf)))

  private def checkLabelInfo(node: RDFNode)(pair: (ShapeMapLabel, Info)): CheckTyping = {
    val (label, info) = pair
    checkNodeShapeMapLabel(node, label, info)
  }

  private def checkNodeShapesMap(node: RDFNode, shapesMap: Map[ShapeMapLabel, Info]): CheckTyping =
    checkAll(shapesMap.map(checkLabelInfo(node)).toList).flatMap(ts => combineTypings(ts))

  private def getShape(label: ShapeLabel): Check[ShapeExpr] =
    schema
      .getShape(label)
      .fold(
        e => err(ShExError.LabelNotFound(label, e, schema.labels)),
        ok(_)
      )

  private def checkNodeShapeLabel(node: RDFNode, shape: ShapeLabel): CheckTyping =
    cond(
      verifyShapeLabel(shape),
      (shapeLabel: ShapeLabel) =>
        checkNodeLabel(node, shapeLabel, None, Visited.empty, WithDescendants.followDescendants),
      err =>
        getTyping.map(_.addNotEvidence(node, ShapeType(ShapeExpr.fail, Some(shape), schema), err))
    )

  private def checkNodeStart(node: RDFNode): CheckTyping = getRDF.flatMap { rdf =>
    schema.start match {
      case None => err(NoStart(node, rdf))
      case Some(shape) =>
        val shapeType = ShapeType(shape, Some(Start), schema)
        val attempt = Attempt(NodeShape(node, shapeType), None, rdf)
        debug(s"${node.show}@Start = ${showSE(shape)}") *>
          runLocalSafeTyping(
            satisfies(node, shape, None, Visited.empty, WithDescendants.followDescendants, attempt),
            _.addType(node, shapeType),
            (err, t) => t.addNotEvidence(node, shapeType, err)
          )
    }
  }

  private def verifyShapeLabel(label: ShapeLabel): Check[ShapeLabel] =
    if (schema.labels contains label) ok(label)
    else err(LabelNotFound(label, "", schema.labels))

  private def checkNodeLabelSafe(
      node: RDFNode,
      label: ShapeLabel,
      se: ShapeExpr,
      ext: Option[Neighs],
      visited: Visited,
      withDescendants: WithDescendants
  ): CheckTyping = {
    val shapeType = ShapeType(se, Some(label), schema)
    getRDF.flatMap { rdf =>
      val attempt = Attempt(NodeShape(node, shapeType), None, rdf)
      getTyping.flatMap(t =>
        runLocalTyping(
          bind(
            checkOptSemActs(attempt, node, schema.startActs),
            satisfies(node, se, ext, visited, withDescendants, attempt)
          ),
          t => t.addType(node, shapeType)
        )
          .handleErrorWith(e =>
            debug(
              s"checkNodeLabelSafe(${node.show}@${schema.qualify(label)} failed. Checking if there are descendants with ${withDescendants.show}"
            ) *>
              (withDescendants match {
                case NoDescendants     => err(e)
                case FollowDescendants => checkDescendants(attempt, node, se, ext, visited)(e)
              })
          )
          .handleErrorWith(err => ok(t.addNotEvidence(node, shapeType, err)))
      )
    }
  }

  private def addNot(node: RDFNode, label: ShapeLabel, typing: ShapeTyping)(
      err: ShExError
  ): CheckTyping = {
    val shapeType = ShapeType(ShapeExpr.fail, Some(label), schema)
    ok(typing.addNotEvidence(node, shapeType, err))
  }

  private[validator] def checkNodeLabel(
      node: RDFNode,
      label: ShapeLabel,
      ext: Option[Neighs],
      visited: Visited,
      withDescendants: WithDescendants
  ): CheckTyping =
    getTyping.flatMap(typing =>
      getNodesPrefixMap.flatMap(nodesPrefixMap =>
        infoType(
          s"nodeLabel(${node.show},${label.show}). ${showVisited(visited)}, ext: ${showExt(ext)}, withDescendants: ${withDescendants.show}"
        ) *>
          (if (visited.contains(node, label) && !withDescendants.isNoDescendants) {
             debug(
               s"checkNodeLabel(${node.show},${label.show}). Visited already contains label ${label.show}. ${showVisited(visited)} Ext: ${showExt(ext)}"
             ) *>
               // errStr(s"checkNodeLabel(${node.show},${label.show}). Visited already contains label ${label.show}. Visited: ${visited} Ext: ${showExt(ext)}")
               ok(typing)
           } else if (typing.hasInfoAbout(node, label))
             debug(
               s"checkNodeLabel(${node.show},${label.show}). Typing already contains label ${label.show}. ${visited
                   .show(schema)}, Ext: ${showExt(ext)}"
             ) *>
               ok(typing)
           else {
             cond(
               getShape(label),
               (se: ShapeExpr) =>
                 checkNodeLabelSafe(
                   node,
                   label,
                   se,
                   ext,
                   visited.add(node, label),
                   withDescendants
                 ),
               addNot(node, label, typing)
             )
           }).flatMap(newTyping =>
            infoTyping(
              newTyping,
              s"Result of checkNodeLabel(${node.show},${label.show})",
              nodesPrefixMap
            ) *>
              ok(newTyping)
          )
      )
    )

  private def satisfies(
      node: RDFNode,
      s: ShapeExpr,
      ext: Option[Neighs],
      visited: Visited,
      withDescendants: WithDescendants,
      attempt: Attempt
  ): CheckTyping =
    debug(
      s"satisfies(${node.show}@${showSE(s)}, withDescendants: ${withDescendants.show}"
    ) *>
      getNodesPrefixMap
        .flatMap(nodesPrefixMap =>
          s match {
            case so: ShapeOr => checkOr(node, so.shapeExprs, ext, visited, withDescendants, attempt)
            case sa: ShapeAnd =>
              checkAnd(node, sa.shapeExprs, ext, visited, withDescendants, attempt)
            case sn: ShapeNot =>
              checkNot(node, sn.shapeExpr, ext, visited, withDescendants, attempt)
            case nc: NodeConstraint => nodeValidator.checkNodeConstraint(attempt, node, nc)
            case s: Shape           => checkShape(node, s, ext, visited, withDescendants, attempt)
            case sr: ShapeRef =>
              checkRef(node, sr.reference, ext, visited, withDescendants, attempt)
            case se: ShapeExternal =>
              checkExternal(node, se, ext, visited, withDescendants, attempt)
            case sd: ShapeDecl =>
              err[ShapeTyping](
                NotImplemented(
                  node,
                  s"Not implemented abstract shapes in ShEx 2.1. ${showSE(sd)}",
                  attempt
                )
              )
          }
        )
        .flatMap(typing =>
          infoTyping(typing, s"end of satisfies(${node.show},${showSE(s)})", schema.prefixMap) *>
            ok(typing)
        )

  private def checkDescendants(
      attempt: Attempt,
      node: RDFNode,
      se: ShapeExpr,
      ext: Option[Neighs],
      visited: Visited
  )(e: ShExError): CheckTyping =
    debug(s"checkDescendants(${node.show}@${showSE(se)}, ${showVisited(visited)}") *>
      getDescendants(se).flatMap { descendants =>
        val nonAbstractDs = descendants.filter(d =>
          schema.isNonAbstract(d) &&
            !visited.contains(node, d)
        )
        if (nonAbstractDs.isEmpty)
          debug(
            s"checkDescendants(${node.show}@${showSE(se)}. Empty descendants found!, real descendants: ${showLabels(descendants)}, ${showVisited(visited)}"
          ) *>
            err(e)
        else {
          val cs = nonAbstractDs.toList.map(lbl =>
            checkRef(node, lbl, ext, visited, NoDescendants, attempt)
          )
          // debug(s"checkDescendants(${node.show}@${showSE(se)}, attempt: ${attempt.show}") *>
          infoType(
            s"Checking descendants(${node.show}@${showSE(se)}) ${nonAbstractDs.map(schema.qualify(_)).mkString(",")}"
          ) *>
            checkSome(
              cs,
              ShapeExprFailedAndNoDescendants(attempt, node, se, e, nonAbstractDs, schema)
            ).flatMap(t1 =>
              addEvidence(attempt.nodeShape, s"${node.show} passes one descendant").flatMap(t2 =>
                combineTypings(t1, t2)
              )
            )
        }
      }

  private def infoType(msg: String): Check[Unit] =
    getTyping.flatMap(t => infoTyping(t, s"$msg || Typing = ", schema.prefixMap))

  private def checkAnd(
      node: RDFNode,
      ses: List[ShapeExpr],
      ext: Option[Neighs],
      visited: Visited,
      withDescendants: WithDescendants,
      attempt: Attempt
  ): CheckTyping =
    debug(s"And($node, ${ses.map(showSE(_)).mkString(",")})") *>
      checkAll(ses.map(se => satisfies(node, se, ext, visited, withDescendants, attempt))).flatMap(
        ts => combineTypings(ts)
      )

  private def checkOr(
      node: RDFNode,
      ses: List[ShapeExpr],
      ext: Option[Neighs],
      visited: Visited,
      withDescendants: WithDescendants,
      attempt: Attempt
  ): CheckTyping =
    debug(s"OR(${showSEs(ses)}, ext: ${showExt(ext)}") *>
      checkSome(
        ses.map(se => satisfies(node, se, ext, visited, withDescendants, attempt)),
        StringError(
          s"None of the alternatives of OR(${ses.map(showSE(_)).mkString(",")}) is valid for node ${node.show}"
        )
      ).flatMap(t1 =>
        addEvidence(attempt.nodeShape, s"${node.show} passes OR").flatMap(t2 =>
          combineTypings(t1, t2)
        )
      )

  private def checkNot(
      node: RDFNode,
      s: ShapeExpr,
      ext: Option[Neighs],
      visited: Visited,
      withDescendants: WithDescendants,
      attempt: Attempt
  ): CheckTyping = {
    val parentShape = attempt.nodeShape.st
    val check: CheckTyping = satisfies(node, s, ext, visited, withDescendants, attempt)
    val handleError: ShExError => Check[ShapeTyping] = e =>
      for {
        t1 <- addNotEvidence(
          NodeShape(node, ShapeType(s, None, schema)),
          e,
          s"${node.show} does not satisfy ${showSE(s)}. Negation declared in ${parentShape.show}. Error: $e"
        )
        t2 <- addEvidence(attempt.nodeShape, s"${node.show} satisfies not(${showSE(s)})")
        t <- combineTypings(List(t1, t2))
      } yield t
    val handleNotError: ShapeTyping => Check[ShapeTyping] = t =>
      errStr(s"Failed NOT(${showSE(s)}) because node ${node.show} satisfies it")
    cond(check, handleNotError, handleError)
  }

  private def checkShapeDecl(
      node: RDFNode,
      sd: ShapeDecl,
      ext: Option[Neighs],
      visited: Visited,
      withDescendants: WithDescendants,
      attempt: Attempt
  ): CheckTyping =
    debug(
      s"ShapeDecl(${node.show}, ${showSE(sd)}), withDescendants: ${withDescendants.show} ${showVisited(visited)}"
    ) *>
      (withDescendants match {
        case NoDescendants => satisfies(node, sd.shapeExpr, ext, visited, NoDescendants, attempt)
        case FollowDescendants =>
          checkDescendants(attempt, node, sd, ext, visited)(
            StringError(
              s"Abstract shape ${showSE(sd)} is not satisfied by any descendant for node ${node.show}"
            )
          )
      })

  private def checkNonAbstractsInTyping(
      node: RDFNode,
      t: ShapeTyping,
      nonAbstracts: Set[ShapeLabel],
      ext: Option[Neighs],
      visited: Visited,
      attempt: Attempt,
      s: ShapeLabel
  ): Check[Unit] = {
    def checkNonAbstractInTyping(nonAbstract: ShapeLabel): Check[Unit] =
      if (t.hasType(node, nonAbstract)) ok(())
      else {
        // checkNodeLabel(node,nonAbstract,ext,visited,false).flatMap(newt =>
        // if (newt.hasType(node, nonAbstract)) ok(())
        // else
        errStr(s"nonAbstract ${schema
            .qualify(nonAbstract)} is not in typing for node ${node.show}. Typing = ${t.show}")
        // )
      }

    getRDF.flatMap(rdf =>
      debug(
        s"checking nonAbstracts(${node.show}@${schema.qualify(s)}): ${showLabels(nonAbstracts)}"
      ) *>
        (if (nonAbstracts.isEmpty) err(AbstractShapeErr(node, s, rdf))
         else
           checkSome(
             nonAbstracts.toList.map(checkNonAbstractInTyping),
             AbstractShapeErr(node, s, rdf)
           ))
    )
  }

  private def getDescendants(s: ShapeExpr): Check[Set[ShapeLabel]] = s.id match {
    case None => ok(Set())
    case Some(lbl) =>
      fromIO(schema.inheritanceGraph.descendantsByEdgtypes(lbl, Set(Extends, References)))
  }

  private def checkHasType(node: RDFNode, typing: ShapeTyping, attempt: Attempt)(
      shapeLabel: ShapeLabel
  ): Check[Unit] =
    if (typing.hasNoType(node, shapeLabel)) {
      getRDF.flatMap(rdf => err(HasNoType(node, shapeLabel, typing, attempt, rdf)))
    } else ok(())

  private def checkRef(
      node: RDFNode,
      ref: ShapeLabel,
      ext: Option[Neighs],
      visited: Visited,
      withDescendants: WithDescendants,
      attempt: Attempt
  ): CheckTyping =
    getTyping.flatMap(typing =>
      getNodesPrefixMap.flatMap(nodesPrefixMap =>
        infoType(
          s"Ref: ${node.show}@${schema.qualify(ref)}, ext: ${showExt(ext)}, ${showVisited(visited)}, withDescendants: ${withDescendants.show}"
        ) *>
          checkNodeLabel(node, ref, ext, visited, withDescendants).flatMap(t =>
            checkHasType(node, t, attempt)(ref).flatMap(_ => ok(t))
          )
      )
    )

  private def getExternalShape(se: ShapeExternal): Check[ShapeExpr] = se.id match {
    case None        => err(NoLabelExternal(se))
    case Some(label) => fromIO(externalResolver.getShapeExpr(label, se.annotations))
  }

  private def checkExternal(
      node: RDFNode,
      se: ShapeExternal,
      ext: Option[Neighs],
      visited: Visited,
      withDescendants: WithDescendants,
      attempt: Attempt
  ): CheckTyping =
    for {
      externalShape <- getExternalShape(se)
      rdf <- getRDF
      newAttempt = Attempt(
        NodeShape(node, ShapeType(externalShape, se.id, schema)),
        attempt.path,
        rdf
      )
      t <- satisfies(node, externalShape, ext, visited, withDescendants, newAttempt)
    } yield t

  // TODO: We changed the definition of allPaths...so this code may be broken...
  private def allPaths(s: Shape): Check[Set[Path]] = fromEitherString(allPaths(s, schema))

  private def checkShape(
      node: RDFNode,
      s: Shape,
      ext: Option[Neighs],
      visited: Visited,
      withDescendants: WithDescendants,
      attempt: Attempt
  ): CheckTyping =
    infoType(s"Shape(${node.show}@${showSE(s)}) ext: ${showExt(ext)}") *>
      allPaths(s).flatMap(paths =>
        infoType(s"Shape(${node.show}@${showSE(s)}), paths: ${paths.show}") *>
          getNeighs(node, ext).flatMap { neighs =>
            val (neighsInPaths, otherNeighs) = neighs.partitionByPaths(paths)
            val otherNeighsDirect = otherNeighs.filterDirect
            if (s.isClosed && otherNeighsDirect.nonEmpty) {
              debug(
                s"Shape(${node.show}@${showSE(s)}) closed condition failed, extra-neighs: $otherNeighsDirect.showQualified(schema.prefixMap)}"
              ) *>
                debug(s"neighsInPaths: ${neighsInPaths.showQualified(schema.prefixMap)}") *>
                getRDF.flatMap(rdf =>
                  err(ExtraPropertiesClosedShape(node, otherNeighs.getPredicates(), s, rdf))
                )
            } else {
              val entries = getEntries(neighsInPaths)
              debug(
                s"Shape...obtaining shapeExprs...neighs: ${neighs.showQualified(schema.prefixMap)}"
              ) *>
                getAvailableShapeExprs(s).flatMap(ses =>
                  getAvailablePaths(ses).flatMap { availablePaths =>
                    val ps = partsOver(entries, availablePaths)
                    def processLine(line: List[Set[Entry[Path, RDFNode]]]): CheckTyping = {
                      val neighsShape = entries2Neighs(line.head)
                      val extended =
                        line.zip(ses).tail.map { case (ns, se) => (se, entries2Neighs(ns)) }
                      checkPartitionNeighs(attempt, node, s, neighsShape, extended, visited)
                    }
                    debug(s"Available ShapeExprs: ${showSEs(ses)}") *>
                      debug(
                        s"Available paths: ${availablePaths.map(_.values.map(_.show).mkString(",")).mkString("|")}"
                      ) *>
                      (ps match {
                        case p #:: rs if rs.isEmpty =>
                          debug(s"One single line---") *> processLine(p)
                        case _ =>
                          checkSomeLazyList(
                            ps.map(processLine),
                            StringError(s"No partition conforms")
                          )
                      })
                  }
                )
            }
          }
      )

  def checkPartitionNeighs(
      attempt: Attempt,
      node: RDFNode,
      s: Shape,
      neighsShape: Neighs,
      extended: List[(ShapeExpr, Neighs)],
      visited: Visited
  ): CheckTyping =
    infoType(s"checkPartitionNeighs(${node.show}@${showSE(s)} with neighs: ${neighsShape
        .showQualified(schema.prefixMap)} | extended: ${extended
        .map { case (se, ns) => showSE(se) + " [" + ns.showQualified(schema.prefixMap) + "]" }
        .mkString("|")} | ${visited.show(schema)}") *>
      checkShapeBase(attempt, node, s, Some(neighsShape), addVisited(visited, node, s.id)).flatMap {
        t =>
          val cs = extended.map { case (se, neighs) =>
            val newVisited = addVisited(visited, node, s.id)
            satisfies(node, se, Some(neighs), newVisited, NoDescendants, attempt)
          }
          checkAll(cs).flatMap(ts => combineTypings(t :: ts))
      }

  private def addVisited(visited: Visited, node: RDFNode, s: Option[ShapeLabel]): Visited =
    s.fold(visited)(visited.add(node, _))

  private def showVisited(visited: Visited): String =
    visited.show(schema)

  private case class E(key: Path, value: RDFNode) extends PartitionUtils.Entry[Path, RDFNode]

  def entries2Neighs(set: Set[Entry[Path, RDFNode]]): Neighs = {
    val m = set
      .map(x => (x.key, x.value))
      .groupBy(_._1)
      .toMap
    Neighs(mapValues(m)(_.map(_._2)))
  }

  def getEntries(neighs: Neighs): Set[Entry[Path, RDFNode]] =
    neighs.m
      .map { case (p, ns) => ns.flatMap(n => List((p, n))) }
      .flatten
      .map { case (p, n) => E(p, n) }
      .toSet

  def getAvailableShapeExprs(s: Shape): Check[List[ShapeExpr]] =
    s._extends match {
      case None => ok(List(s))
      case Some(es) =>
        errStr(s"Shape ${showSE(s)} has extends which are not implemented in version 2.1")
    }
    // s.getExtend.map(getShape).sequence.map(s :: _ )

  def getAvailablePaths(ses: List[ShapeExpr]): Check[List[Available[Path]]] =
    ses.map(se => getPaths(se, schema).map(Available.apply)).sequence

  private def checkNeighsShape(
      attempt: Attempt,
      node: RDFNode,
      neighs: Neighs,
      s: Shape,
      visited: Visited
  ): CheckTyping =
    if (s.hasRepeatedProperties(schema))
      checkNeighsShapeWithTable(attempt, node, neighs, s, visited)
    else {
      // TODO
      checkNeighsShapeWithTable(attempt, node, neighs, s, visited)
    }

  private def checkNeighsShapeWithTable(
      attempt: Attempt,
      node: RDFNode,
      neighs: Neighs,
      s: Shape,
      visited: Visited
  ): CheckTyping =
    for {
      _ <- debug(s"""|checkNeighsShapeWithTable: ${node.show}@${showSE(s)} with neighs:${neighs
                      .showQualified(schema.prefixMap)}""".stripMargin)
      tableRbe <- mkTable(s.expression, s.extra.getOrElse(List()), schema.prefixMap)
      (cTable, rbe) = tableRbe
      // _ <- info(s"cTable: $cTable")
      bagChecker = IntervalChecker(rbe)
      csRest <- calculateCandidates(neighs, cTable)
      (candidates, rest) = csRest
      _ <- checkRests(rest, s.extraPaths, s.isClosed, ignoredPathsClosed, s, attempt)
      paths <- fromEither(s.paths(schema).leftMap(StringError(_)))
      _ <- debug(s"Checking closed condition with paths=${paths.show}, neighs=${neighs
          .showQualified(schema.prefixMap)}. Closed: ${s.closed}")
      _ <- {
        if (s.isClosed) {
          checkNoStrangeProperties(node, paths.toList, s, attempt, neighs)
        } else ok(())
      }
      _ <- infoType(
        s"checkNeighsShapeWithTable(${node.show}@${showSE(s)}): Before checkCandidates: ${candidates.cs
            .map(_.show)
            .mkString(",")}, Table:${cTable.show}"
      )
      typing <- checkCandidates(attempt, bagChecker, cTable, node, Some(neighs), visited)(
        candidates
      )
      _ <- infoType(s"checkNeighsShapeWithTable(${node.show}@${showSE(s)}): After checkCandidates")
      _ <- checkOptSemActs(attempt, node, s.actions)
    } yield
    // println(s"End of checkShape(attempt=${attempt.show},node=${node.show},shape=${s.show})=${typing.show}")
    typing

  /* Check a shape without extends and restricts
   *  It doesn't check closed or EXTRA
   * */
  private def checkShapeBase(
      attempt: Attempt,
      node: RDFNode,
      s: Shape,
      ext: Option[Neighs],
      visited: Visited
  ): CheckTyping =
    debug(s"checkShapeBase(${node.show}@{${showSE(s)}}, flatShape? ${s
        .isFlatShape(schema)}, ${showVisited(visited)}, ext: ${showExt(ext)}") *>
      (s match {
        case _ if s.isEmpty =>
          addEvidence(attempt.nodeShape, s"Node ${node.show} conforms empty shape")
        case _ if s.isFlatShape(schema) =>
          for {
            flatShape <- fromEitherString(s.flattenShape(schema))
            pm <- getNodesPrefixMap
            typing <-
              FlatShapeValidator(pm, schema.prefixMap, builder, schema).checkFlatShape(
                attempt,
                node,
                flatShape.withClosed,
                ext
              )
          } yield typing
        case _ =>
          for {
            paths <- getPaths(s, schema)
            neighs <- getNeighPaths(node, paths, ext)
            typing <- checkNeighsShape(attempt, node, neighs, s, visited)
          } yield typing
      })

  private def checkNoStrangeProperties(
      node: RDFNode,
      paths: List[Path],
      shape: Shape,
      attempt: Attempt,
      neighs: Neighs
  ): Check[Unit] =
    for {
      s <- getNotAllowedPredicates(node, paths, neighs)
      rdf <- getRDF
      // _ <- debug(s"NotAllowedPredicates: ${s}")
      _ <- checkCond(
        s.isEmpty,
        attempt,
        ExtraPropertiesClosedShape(node, s, shape, rdf),
        "Closed properties with no extra property"
      )
    } yield ()

  private def checkOptSemActs(
      attempt: Attempt,
      node: RDFNode,
      maybeActs: Option[List[SemAct]]
  ): Check[Unit] =
    maybeActs match {
      case None     => ok(())
      case Some(as) => checkSemActs(attempt, node, as)
    }

  private def checkSemActs(attempt: Attempt, node: RDFNode, as: List[SemAct]): Check[Unit] =
    for {
      _ <- checkAll(as.map(checkSemAct(attempt, node, _)))
    } yield ()

  private def checkSemAct(attempt: Attempt, node: RDFNode, a: SemAct): Check[Unit] =
    for {
      rdf <- getRDF
      eitherResult <- runAction(a.name, a.code, node, rdf)
      _ <- fromEither(eitherResult.leftMap(exc => SemanticActionException(attempt, node, a, exc)))
    } yield ()

  private def runAction(
      name: IRI,
      code: Option[String],
      node: RDFNode,
      rdf: RDFReader
  ): Check[Either[Throwable, Unit]] = {
    val unit: Either[Throwable, Unit] = Right(())
    // println(s"Semantic action: $name/$code")
    for {
      r <- name match {
        case TestSemanticAction.`iri` =>
          fromIO(
            TestSemanticAction.runAction(code.getOrElse(""), node, rdf).attempt
          )
        case _ =>
          addAction2Log(Action(name, code))
          ok(unit)
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
        case (Left(e1), _)          => Left(e1)
        case (_, Left(e2))          => Left(e2)
        case (Right(()), Right(())) => Right(())
      }
    val ts: List[Either[ShExError, Unit]] =
      rests.cs.map(checkRest(_, extras, isClosed, ignoredPathsClosed, shape, attempt))
    val r: Either[ShExError, Unit] = ts.foldLeft(zero)(combine)
    r.fold(err(_), _ => ok(()))
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
        ClosedShapeWithRests(shape, rest, attempt, ignoredPathsClosed, extras).asLeft
      }
    } else ().asRight
  }

  private def mkTable(
      maybeTe: Option[TripleExpr],
      extra: List[IRI],
      prefixMap: PrefixMap
  ): Check[(CTable, Rbe_)] =
    maybeTe match {
      case None => ok((CTable.empty, Empty))
      case Some(te) =>
        fromEitherString(
          for {
            pair <- CTable.mkTable(te, extra, schema.tripleExprMap, prefixMap)
          } yield pair
        )
    }

  /** Calculates the sequence of candidates
    * Example: Neighs (p,x1),(p,x2),(q,x2),(r,x3)
    *   Table: { constraints: C1 -> IRI, C2 -> ., paths: p -> List(C1,C2), q -> C1 }
    *   Result: x1
    * @param neighs
    * @param table
    * @return a tuple (cs,rs) where cs is the list of candidates and rs is the nodes that didn't match any
    */
  private def calculateCandidates(
      neighs: Neighs,
      table: CTable
  ): Check[(Candidates, NoCandidates)] = {
    val candidates = table.neighs2Candidates(neighs.toList)
    val (cs, rs) = candidates.cs.partition(matchable)
    // println(s"Candidates partitioned: cs:\n${cs.map(_.show).mkString(s"\n")}\nrs:${rs.map(_.show).mkString(s"\n")}\n")
    ok((Candidates(cs), NoCandidates(rs.map(_.arc))))
  }

  private def matchable(c: Candidate): Boolean =
    c.crefs.nonEmpty

  private[validator] def checkCandidates(
      attempt: Attempt,
      bagChecker: BagChecker_,
      table: CTable,
      node: RDFNode,
      ext: Option[Neighs],
      visited: Visited
  )(candidates: Candidates): CheckTyping = {

    val as = candidates.getCandidateLines()
    as.length match {
      case 1 => // Deterministic
        debug(s"Deterministic invoking checkCandidateLine") *>
          checkCandidateLine(attempt, bagChecker, table, node, ext, visited)(as.head).flatMap { t =>
            debug(s"After checking candidate line") *> ok(t)
          }
      case 0 =>
        getRDF.flatMap(rdf => err(NoCandidateLine(attempt, table, node, rdf)))
      case n =>
        val checks: List[CheckTyping] =
          as.map(checkCandidateLine(attempt, bagChecker, table, node, ext, visited)(_))
        getRDF.flatMap(rdf =>
          checkSome(checks, NoCandidate(attempt, bagChecker, as, table, node, rdf))
        )
    }
  }

  private def checkCandidateLine(
      attempt: Attempt,
      bagChecker: BagChecker_,
      table: CTable,
      node: RDFNode,
      ext: Option[Neighs],
      visited: Visited
  )(
      cl: CandidateLine
  ): CheckTyping = {
    val bag = cl.mkBag
    bagChecker
      .check(bag, false)
      .fold(
        e =>
          getRDF.flatMap(rdf =>
            err(ErrRBEMatch(attempt, cl, table, bag, bagChecker.rbe, e.head, node, rdf))
          ),
        bag => {
          val nodeConstraints = cl.nodeConstraints(table)
          val checkNodeConstraints: List[CheckTyping] = nodeConstraints.map { case (node, pair) =>
            val (shapeExpr, maybeSemActs) = pair
            satisfies(node, shapeExpr, None, visited, WithDescendants.followDescendants, attempt)
              .flatMap(t => checkOptSemActs(attempt, node, maybeSemActs).map(_ => t))
          }
          getTyping.flatMap(typing =>
            checkAll(checkNodeConstraints).flatMap(ts => combineTypings(typing :: ts))
          )
        }
      )
  }

  // Public methods

  /** Validate a node against the START declaration
    */
  def validateNodeStart(rdf: RDFReader, node: IRI, verbose: VerboseLevel): IO[Result] =
    runValidator(checkNodeStart(node), rdf, verbose)

  /** Validate a node following target declarations.
    * This methods follows SHACL convention and could be deprecated in the future
    */
  def validateNodeDecls(rdf: RDFReader, verbose: VerboseLevel): IO[Result] =
    runValidator(checkTargetNodeDeclarations, rdf, verbose)

  /** Validate a node against a shape
    */
  def validateNodeShape(
      rdf: RDFReader,
      node: IRI,
      shape: String,
      verbose: VerboseLevel
  ): IO[Result] =
    ShapeLabel
      .fromString(shape)
      .fold(
        e => IO.raiseError(StringError(s"Can not obtain label from $shape")),
        label => runValidator(checkNodeShapeLabel(node, label), rdf, verbose)
      )

  /** Validate a node against a shape map
    */
  def validateShapeMap(rdf: RDFReader, shapeMap: FixedShapeMap, verbose: VerboseLevel): IO[Result] =
    verbose.info(s"ValidateShapeMap. Validator version: 2.1") *>
      runValidator(checkShapeMap(shapeMap), rdf, verbose)

  /** Execute the validator with a given checker
    * param chk Checker
    * param rdf RDFReader
    * verbose boolean flag to show internal messages
    */
  def runValidator(chk: Check[ShapeTyping], rdf: RDFReader, verbose: VerboseLevel): IO[Result] =
    for {
      r <- runCheck(chk, rdf, verbose)
      pm <- rdf.getPrefixMap
    } yield cnvResult(r, rdf, pm)

  private def cnvResult(
      r: CheckResult[ShExError, ShapeTyping, Log],
      rdf: RDFReader,
      rdfPrefixMap: PrefixMap
  ): Result =
    Result(
      for {
        shapeTyping <- r.toEither
        result <- shapeTyping.toShapeMap(rdfPrefixMap, schema.prefixMap).leftMap(StringError.apply)
      } yield (r.log, result)
    )

  private def showSEs(ses: List[ShapeExpr]): String = ses.map(showSE(_)).mkString(",")
  private def showExt(ext: Option[Neighs]): String =
    ext.map(_.showQualified(schema.prefixMap)).getOrElse("None")
  private def showLs[A: Show](ls: Iterable[A]): String = ls.map(_.show).mkString(",")
  private def showLabels(ls: Set[ShapeLabel]): String =
    s"[${ls.map(schema.qualify(_)).mkString(",")}]"

}
