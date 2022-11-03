package es.weso.shex.validator

import cats._
import cats.data._
import cats.implicits._
// import es.weso.checking.CheckerCats
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.IRI
// import es.weso.shex.validator.Action._
import es.weso.shex.validator.Context._
import cats.effect.IO
import fs2.Stream
import es.weso.rdf.nodes.RDFNode
import es.weso.shex.Direct
import es.weso.shex.Inverse
import es.weso.shex.Path
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes.BNode
import es.weso.shex.ShapeLabel
import es.weso.rdf.RDFBuilder
import es.weso.rdf.PrefixMap
import es.weso.shex.ResolvedSchema
import es.weso.shapemaps.ShapeMapLabel
import es.weso.shex.Shape
import es.weso.shex.BNodeLabel
import es.weso.shex.IRILabel
import es.weso.rdf.PREFIXES._
import es.weso.utils.internal.CollectionCompat.LazyList
import ValidationLog._
import es.weso.utils.VerboseLevel
import es.weso.shex.ShapeExpr
import es.weso.shex.validator.ShExError.AbstractShapeErr

case class ConfigEnv(cfg: ShExConfig, env: Context)
case class State(typing: ShapeTyping)

trait ShExChecker {

//  val builder: RDFBuilder

  type Config = ShExConfig
  type Env = Context
  type Err = ShExError
  type Evidence = (NodeShape, String)
  type Log = ValidationLog

  type Base[A] = IndexedReaderWriterStateT[IO, ConfigEnv, Log, State, State, A]
  type Check[A] = EitherT[Base, Err, A]
  type CheckTyping = Check[ShapeTyping]

  def getConfigEnv: Check[ConfigEnv] =
    fromBase(IndexedReaderWriterStateT.ask)

  def getConfig: Check[Config] = getConfigEnv.map(_.cfg)

  def getEnv: Check[Env] = getConfigEnv.map(_.env)

  def fromBase[A](b: Base[A]): Check[A] =
    EitherT.liftF(b)

  def fromIOUnsafe[A](io: IO[A]): Check[A] =
    fromBase(IndexedReaderWriterStateT.liftF(io))

  def addLog(log: Log): Check[Unit] =
    fromBase(IndexedReaderWriterStateT.tell(log))

  def addAction2Log(a: Action): Check[Unit] = {
    val vl = ValidationLog(List(a), List())
    addLog(vl)
  }

  def local[A](f: Env => Env)(comp: Check[A]): Check[A] = {
    def ff(ce: ConfigEnv): ConfigEnv =
      ce.copy(env = f(ce.env))
    val r: Base[Either[Err, A]] = comp.value.local(ff)
    EitherT(r)
  }

  def ok[A](x: A): Check[A] =
    EitherT.pure[Base, Err](x)

  def err[A](e: Err): Check[A] =
    EitherT.left[A](mkErr[Base](e))

  def fromEither[A](e: Either[Err, A]): Check[A] = EitherT.fromEither[Base](e)

  def fromEitherIO[A](e: EitherT[IO, Err, A]): Check[A] = {
    val ea: Check[Either[Err, A]] = EitherT.liftF(IndexedReaderWriterStateT.liftF(e.value))
    for {
      either <- ea
      r <- either.fold(err(_), ok)
    } yield r
  }

  def orElse[A](c1: Check[A], c2: => Check[A]): Check[A] =
    c1.orElse(c2)

  def checkSome[A](cs: List[Check[A]], errorIfNone: Err): Check[A] = {
    lazy val z: Check[A] = err(errorIfNone)
    def comb(c1: Check[A], c2: Check[A]) = orElse(c1, c2)
    cs.foldRight(z)(comb)
  }

  def checkSome[A](cs: LazyList[Check[A]])(implicit ev: Monoid[Err]): Check[A] = {
    lazy val z: Check[A] = err(ev.empty)
    def comb(c1: Check[A], c2: Check[A]) = orElse(c1, c2)
    cs.foldRight(z)(comb)
  }

  def checkSomeLazyList[A](cs: LazyList[Check[A]], errIfNone: => Err): Check[A] =
    cs.foldRight(err[A](errIfNone)) { case (c1, c2) =>
      orElse(c1, c2)
    }

  /** Given a computation check that returns a pair of value and a flag, returns the first value whose flag is true
    * If none is true, returns the value of the computation parameter last
    * @param ls
    * @param check
    * @param last
    * @tparam A
    * @tparam B
    * @tparam F
    * @return
    */
  def checkSomeFlag[A, B, F[_]: Monad](
      ls: => LazyList[A],
      check: A => F[(B, Boolean)],
      last: F[(B, Boolean)]
  ): F[(B, Boolean)] = {
    val z: Eval[F[(B, Boolean)]] = Eval.later(last)
    def cmb(x: A, next: Eval[F[(B, Boolean)]]): Eval[F[(B, Boolean)]] =
      Eval.later(
        for {
          r <- check(x)
          n <-
            if (r._2) Monad[F].pure(r)
            else next.value
        } yield n
      )
    Foldable[LazyList].foldRight(ls, z)(cmb).value
  }

  /** Checks some values. Returns the first that passes or None
    */
  def checkSomeFlagValue[A, B](
      ls: => LazyList[A],
      check: A => Check[B],
      last: Check[B]
  ): Check[(B, Option[A])] = {
    val z: Eval[Check[(B, Option[A])]] = Eval.later(last.map(x => (x, None)))
    def cmb(x: A, next: Eval[Check[(B, Option[A])]]): Eval[Check[(B, Option[A])]] =
      Eval.later(check(x).map(r => (r, Some(x))).orElse(next.value))
    Foldable[LazyList].foldRight(ls, z)(cmb).value
  }

  def checkSomeFlagCount[A, B: Monoid](
      ls: => LazyList[A],
      check: A => Check[(B, Boolean)],
      last: B
  ): Check[(B, Int)] = {
    val z: Eval[Check[(B, Int)]] = Eval.later(ok((last, 0)))
    def cmb(x: A, next: Eval[Check[(B, Int)]]): Eval[Check[(B, Int)]] =
      Eval.later(
        for {
          r1 <- check(x)
          r2 <- next.value
        } yield
          if (r1._2) (r1._1 |+| r2._1, 1 + r2._2)
          else r2
      )
    Foldable[LazyList].foldRight(ls, z)(cmb).value
  }

  def checkAllFlag[A, B: Monoid, F[_]: Monad](
      ls: => LazyList[A],
      check: A => F[(B, Boolean)],
      last: => B
  ): F[(B, Boolean)] = {
    val z: Eval[F[(B, Boolean)]] =
      Eval.later(Monad[F].pure((last, true)))
    def cmb(x: A, next: Eval[F[(B, Boolean)]]): Eval[F[(B, Boolean)]] =
      Eval.later(
        for {
          r <- check(x)
          n <- next.value
        } yield {
          val newR = (n._1 |+| r._1, n._2 && r._2)
          newR
        }
      )
    Foldable[LazyList].foldRight(ls, z)(cmb).value
  }

  def checkAllFailFAtFirstFlag[A, B: Monoid, F[_]: Monad](
      ls: => LazyList[A],
      check: A => F[(B, Boolean)],
      last: => B
  ): F[(B, Boolean)] = {
    val z: Eval[F[(B, Boolean)]] =
      Eval.later(Monad[F].pure((last, true)))
    def cmb(x: A, next: Eval[F[(B, Boolean)]]): Eval[F[(B, Boolean)]] =
      Eval.later(
        for {
          r <- check(x)
          n <-
            if (!r._2) Monad[F].pure(r)
            else
              for {
                v <- next.value
              } yield (v._1 |+| r._1, v._2)
        } yield n
      )
    Foldable[LazyList].foldRight(ls, z)(cmb).value
  }

  def checkSequenceFlag[A: Monoid, F[_]: Monad](
      ls: => List[F[(A, Boolean)]],
      last: A
  ): F[(A, Boolean)] = {
    val z: Eval[F[(A, Boolean)]] = Eval.later(Monad[F].pure((last, true)))
    def cmb(x: F[(A, Boolean)], next: Eval[F[(A, Boolean)]]): Eval[F[(A, Boolean)]] =
      Eval.later(for {
        r1 <- x
        r2 <- next.value
      } yield (r1._1 |+| r2._1, r1._2 && r2._2))
    Foldable[List].foldRight(ls, z)(cmb).value
  }

  /** Run a computation in a local environment. If the computation fails, return the result of calling `safe` function over the current environment
    * @param c computation to run
    * @param f environment
    * @param safe function to call if the computation fails
    * @tparam A
    * @return
    */
  def runLocalSafe[A](c: Check[A], f: Env => Env, safe: (Err, Env) => A): Check[A] = {
    def fnOk(t: A): Check[A] = ok(t)
    def fnErr(err: Err): Check[A] = for {
      t <- getEnv
    } yield safe(err, t)
    cond(local(f)(c), fnOk, fnErr)
  }

  def runLocal[A](c: Check[A], f: Env => Env): Check[A] =
    local(f)(c)

  /** Given a list of checks, return the list of values that pass
    * It never fails (in case of failure, it ignores the value)
    */
  def checkLs[A](cs: List[Check[A]]): Check[List[A]] = {
    lazy val z: Check[List[A]] = ok(List())
    val css: List[Check[List[A]]] = cs.map(c => c.map(List(_)).orElse(z))
    def comb(rest: Check[List[A]], current: Check[List[A]]): Check[List[A]] = for {
      xs <- rest
      ys <- current
    } yield xs ++ ys
    css.foldLeft(z)(comb)
  }

  def checkOneOf[A](cs: List[Check[A]], errNone: Err, errMoreThanOne: List[A] => Err): Check[A] =
    for {
      rs <- checkLs(cs)
      v <- rs.length match {
        case 0 => err[A](errNone)
        case 1 => ok(rs.head)
        case _ => err[A](errMoreThanOne(rs))
      }
    } yield v

  def attempt[A](c: Check[A]): Check[Either[Err, A]] = for {
    v <- MonadError[Check, Err].attempt(c)
  } yield v

  /** Returns the list of values whose computation is successful
    * @param ls list of values
    * @param check computation to check for each value
    * @tparam A type of values
    * @tparam B type returned by computation
    * @return a computation with a list of pairs for whom the computation was successful
    */
  def filterSuccess[A, B](ls: List[A], check: A => Check[B]): Check[List[(A, B)]] = {
    val zero: Check[List[(A, B)]] = ok(List())
    def comb(rest: Check[List[(A, B)]], current: A): Check[List[(A, B)]] = for {
      rs <- rest
      c <- attempt(check(current))
    } yield c match {
      case Left(_)  => rs
      case Right(b) => (current, b) +: rs
    }
    ls.foldLeft(zero)(comb)
  }

  /** Attempts to execute a check
    * If it fails, applies `thenPart` to the result, otherwise applies `elsePart` to the error
    *
    * @param check Computation to check
    * @param thenPart part to be executed when it passes
    * @param elsePart part to be executed when the check fails
    * @tparam A type returned by the computation
    * @tparam B type returned the the condition
    * @return
    */
  def cond[A, B](check: Check[A], thenPart: A => Check[B], elsePart: Err => Check[B]): Check[B] =
    attempt(check).flatMap(_.fold(elsePart(_), thenPart(_)))

  def condFlag[A, B](
      check: Check[A],
      thenPart: A => Check[B],
      elsePart: Err => Check[B]
  ): Check[(B, Boolean)] =
    attempt(check).flatMap(_.fold(elsePart(_).map((_, false)), thenPart(_).map((_, true))))

  def checkList[A, B](ls: List[A], check: A => Check[B]): Check[List[B]] =
    checkAll(ls.map(check))

  /** Checks all elements in a list
    * If any of the elements fails, it fails
    */
  def checkAll[A](xs: List[Check[A]]): Check[List[A]] =
    sequence(xs) // Question: Is this stack safe?

  def sequence[A](xs: List[Check[A]]): Check[List[A]] =
    xs.sequence[Check, A] // Question: Is this stack safe?

  def checkPair1st[A, B](p: (Check[A], B)): Check[(A, B)] = for {
    v <- p._1
  } yield (v, p._2)

  def checkPair2nd[A, B](p: (A, Check[B])): Check[(A, B)] = for {
    v <- p._2
  } yield (p._1, v)

  /*  def optCheck[A,B](
    x: Option[A],
    f: => A => Check[B]): Check[Option[B]] = x match {
    case None => ok(None)
    case Some(v) => f(v).map(Some(_))
  } */

  /** If `c` is some value, applies `check`, otherwise applies `default`
    * @param c Optional value
    * @param check check function
    * @param default value in case there is no option
    * @tparam A
    * @tparam B
    * @return
    */
  def optCheck[A, B](c: Option[A], check: A => Check[B], default: => Check[B]): Check[B] =
    c.fold(default)(check(_))

  def validateCheck(condition: Boolean, e: Err): Check[Unit] =
    if (condition) Monad[Check].pure(())
    else err(e)

  def run[A](c: Check[A])(config: Config)(env: Env): IO[(Log, Either[Err, A])] =
    c.value.run(ConfigEnv(config, env), State(ShapeTyping.emptyShapeTyping)).map { vs =>
      val (log, env, c) = vs
      (log, c)
    }

  def mkErr[F[_]: Applicative](e: Err): F[Err] =
    Applicative[F].pure(e)

  def errStr[A](msg: String): Check[A] =
    info(s"errStr($msg)") *>
      err[A](ShExError.msgErr(msg))

    // TODO: Capture errors in EitherT
  def fromIO[A](io: IO[A]): Check[A] = {
    val r: Check[Either[Throwable, A]] = fromIOUnsafe(io.attempt)
    r.flatMap(e => e.fold(t => err[A](ShExError.ExceptionError(t)), v => ok(v)))
  }

  def fromEitherString[A](e: Either[String, A]): Check[A] =
    fromEither(e.leftMap(ShExError.msgErr(_)))

  def fromStream[A](s: Stream[IO, A]): Check[List[A]] = fromIO(s.compile.toList)

  def fromEitherIOS[A](e: EitherT[IO, String, A]): Check[A] = {
    val ea: Check[Either[String, A]] = fromIO(e.value)
    for {
      either <- ea
      r <- either.fold(errStr(_), ok)
    } yield r
  }

  def info(msg: String): Check[Unit] = getVerbose.flatMap(v => fromIO(v.info(msg)))
  def debug(msg: String): Check[Unit] = getVerbose.flatMap(v => fromIO(v.debug(msg)))
  def step(msg: String): Check[Unit] = getVerbose.flatMap(v => fromIO(v.step(msg)))

  def checkCond(
      condition: Boolean,
      attempt: Attempt,
      error: ShExError,
      evidence: String
  ): CheckTyping = for {
    _ <- validateCheck(condition, error)
    newTyping <- addEvidence(attempt.nodeShape, evidence)
  } yield newTyping

  val iriActions = IRI("http://shex.io/actions/log")

  def addEvidence(nodeShape: NodeShape, msg: String): Check[ShapeTyping] = {
    // println(s"adding evidence...$msg")
    val action = Action(iriActions, Some(s"Evidence added: $nodeShape: $msg"))
    getTyping.flatMap(t =>
      addAction2Log(action) *>
        debug(s"Adding evidence ${nodeShape}: $msg") *>
        ok(t.addEvidence(nodeShape.node, nodeShape.st, msg))
    )
  }

  def addNotEvidence(nodeShape: NodeShape, e: ShExError, msg: String): Check[ShapeTyping] = {
    val action = Action(iriActions, Some(s"Not Evidence: $nodeShape: $msg"))
    val node = nodeShape.node
    val shape = nodeShape.st
    for {
      t <- getTyping
      _ <- addAction2Log(action)
    } yield t.addNotEvidence(node, shape, e)
  }

  /*  def runLocalVisited[A](c: Check[A],
                        visited: Option[ShapeLabel]): Check[A] = {
    runLocal(c, _.addVisited(visited))
  }  */

  /*  def runLocalNeighs[A](c: Check[A],
                        node: RDFNode,
                        neighs: Neighs): Check[A] = {
    def liftedF(c: Context): Context =
       c.addLocalNeighs(node,neighs)
    runLocal(c, liftedF)
  }

  def runLocalNeighsVisited[A](c: Check[A],
                        node: RDFNode,
                        neighs: Neighs,
                        visited: Option[ShapeLabel]): Check[A] = {
    def liftedF(c: Context): Context =
       c.addLocalNeighs(node,neighs).addVisited(visited)
    runLocal(c, liftedF)
  } */

  def runLocalTyping[A](c: Check[A], f: ShapeTyping => ShapeTyping): Check[A] = {
    def liftedF(c: Context): Context = c.updateTyping(f)
    runLocal(c, liftedF)
  }

  def bind[A, Other](c1: Check[Other], c2: Check[A]): Check[A] = c1 >> c2

  def runLocalSafeTyping[A](
      c: Check[A],
      f: ShapeTyping => ShapeTyping,
      safe: (Err, ShapeTyping) => A
  ): Check[A] = {
    def liftedF(c: Context): Context = c.updateTyping(f)
    def liftedSafe(e: Err, c: Context): A = safe(e, c.typing)
    runLocalSafe(c, liftedF, liftedSafe)
  }

  def getRDF: Check[RDFReader] = getConfig.map(_.rdf)
  def getVerbose: Check[VerboseLevel] = getConfig.map(_.verboseLevel)
  def getTyping: Check[ShapeTyping] = getEnv.map(_.typing)

  def getNeighs(node: RDFNode, ext: Option[Neighs]): Check[Neighs] =
    ext match {
      case None =>
        getRDF.flatMap(rdf =>
          outgoingTriples(node, rdf).flatMap(outgoing =>
            incomingTriples(node, rdf).flatMap(incoming =>
              ok(Neighs.fromList(outgoing ++ incoming))
            )
          )
        )
      case Some(ns) => ok(ns)
    }

//  def getVisited: Check[Set[ShapeLabel]] = getEnv.map(_.visited)

  def outgoingTriples(node: RDFNode, rdf: RDFReader): Check[List[Arc]] =
    fromStream(rdf.triplesWithSubject(node))
      .flatMap(ts => ok(ts.map(t => Arc(Direct(t.pred), t.obj))))
      .handleError(_ => List())

  def outgoingTriplesPredicates(node: RDFNode, preds: Set[IRI], rdf: RDFReader): Check[List[Arc]] =
    fromIO(getTriplesWithSubjectPredicates(rdf, node, preds.toList))
      .flatMap(ts => ok(ts.map(t => Arc(Direct(t.pred), t.obj))))
      .handleError(_ => List())

  def incomingTriples(node: RDFNode, rdf: RDFReader): Check[List[Arc]] =
    fromStream(rdf.triplesWithObject(node))
      .flatMap(ts => ok(ts.map(t => Arc(Inverse(t.pred), t.subj))))
      .handleError(_ => List())

  def incomingTriplesPredicates(node: RDFNode, preds: Set[IRI], rdf: RDFReader): Check[List[Arc]] =
    fromStream(rdf.triplesWithObject(node))
      .flatMap(ts =>
        ok(
          ts.filter(t => preds.contains(t.pred))
            .map(t => Arc(Inverse(t.pred), t.subj))
        )
      )
      .handleError(_ => List())

  def getNeighPaths(node: RDFNode, paths: Set[Path], ext: Option[Neighs]): Check[Neighs] =
    ext match {
      case Some(ns) =>
        ok(ns.filterPaths(paths))
      case None =>
        val outgoingPreds = paths.collect { case Direct(p) => p }
        val incomingPreds = paths.collect { case Inverse(p) => p }
        getRDF.flatMap(rdf =>
          incomingTriplesPredicates(node, incomingPreds, rdf).flatMap(incoming =>
            outgoingTriplesPredicates(node, outgoingPreds, rdf).flatMap { outgoing =>
              ok(Neighs.fromList(outgoing ++ incoming))
            }
          )
        )
    }

  /*  def removeShapeType(node: RDFNode, s: ShapeExpr, t: ShapeTyping, schema: ResolvedSchema): CheckTyping =
   getRDF.flatMap(rdf =>
   ok(t.addNotEvidence(node,ShapeType(s,s.id,schema), AbstractShapeErr(node,s,rdf)))) */

  /*         outTriples <- fromIO(getTriplesWithSubjectPredicates(rdf,node,outgoingPredicates.toList))
         strRdf <- fromIO(rdf.serialize("TURTLE"))
         outgoing = outTriples.map(t => Arc(Direct(t.pred), t.obj)).toList
         inTriples <- fromStream(rdf.triplesWithObject(node))
         incoming = inTriples.map(t => Arc(Inverse(t.pred), t.subj)).toList
        } yield {
         val neighs = outgoing ++ incoming
         Neighs.fromList(neighs)
        }
      }
  } */

  def getValuesPath(node: RDFNode, path: Path, ext: Option[Neighs]): Check[Set[RDFNode]] =
    ext match {
      case Some(ns) => ok(ns.values(path))
      case None     => getRDF.flatMap(rdf => fromStream(path.getValues(node, rdf)).map(_.toSet))
    }

  /*  def getValuesPath(node: RDFNode, path: Path, ext: Option[Neighs]): Check[Set[RDFNode]] = for {
    localNeighs <- getLocalNeighs
    vs <- localNeighs.get(node) match {
      case Some(ns) =>
       ok(ns.values(path))
      case None =>     for {
       rdf   <- getRDF
       nodes <- fromStream(path.getValues(node, rdf))
      } yield nodes.toSet
    }
  } yield vs */

  private def getTriplesWithSubjectPredicates(
      rdf: RDFReader,
      node: RDFNode,
      preds: List[IRI]
  ): IO[List[RDFTriple]] =
    node match {
      case _: IRI   => triplesWithSubjectPredicates(node, preds, rdf)
      case _: BNode => triplesWithSubjectPredicates(node, preds, rdf)
      case _        => IO(List())
    }

  private def triplesWithSubjectPredicates(
      n: RDFNode,
      ps: List[IRI],
      rdf: RDFReader
  ): IO[List[RDFTriple]] = {
    // println(s"TriplesWithSubjectPredicates ${n.show}, ${ps.map(_.show).mkString(",")}")
    val ss = mkSeq(
      ps,
      (p: IRI) => {
        // println(s"TriplesWithSubjectPredicates ${n.show}/${p.show}")
        val ts: IO[List[RDFTriple]] = rdf.triplesWithSubjectPredicate(n, p).compile.toList
        // println(s"TriplesWithSubjectPredicates ${n.show}/${p.show} = ${ts.unsafeRunSync().map(_.show).mkString(",")}")
        ts
      }
    )
    ss
  }

  def getNotAllowedPredicates(node: RDFNode, paths: List[Path], neighs: Neighs): Check[Set[IRI]] = {
    def getPredicate: PartialFunction[Path, IRI] = p =>
      p match {
        case Direct(pred) => pred
      }
    ok(
      neighs.filterPathCond(p => !(paths contains p)).toList.map(_.path).toSet.collect(getPredicate)
    )
  }

  def combineTypings(ts: List[ShapeTyping]): Check[ShapeTyping] =
    ok(ShapeTyping.combineTypings(ts))

  def combineTypings(ts: ShapeTyping*): Check[ShapeTyping] =
    ok(ShapeTyping.combineTypings(ts))

  def runCheck[A: Show](
      c: Check[A],
      rdf: RDFReader,
      verbose: VerboseLevel = VerboseLevel.Nothing
  ): IO[CheckResult[ShExError, A, Log]] = {
    val initial: Context = Monoid[Context].empty
    for {
      result <- run(c)(ShExConfig(rdf, verbose))(initial)
    } yield {
      val (log, eitherTyping) = result
      CheckResult(log, eitherTyping)
    }
  }

  private lazy val `sh:targetNode` = sh + "targetNode"

  def mkSeq[A, B](vs: List[A], f: A => IO[List[B]]): IO[List[B]] =
    vs.traverse(f).map(_.flatten)

  def getTargetNodeDeclarations(rdf: RDFReader): Check[List[(RDFNode, ShapeLabel)]] =
    for {
      ts <- fromStream(rdf.triplesWithPredicate(`sh:targetNode`))
      r <- checkAll(ts.map(t => (t.obj, mkShapeLabel(t.subj))).toList.map(checkPair2nd))
    } yield r

  private def mkShapeLabel(n: RDFNode): Check[ShapeLabel] =
    n match {
      case i: IRI   => ok(IRILabel(i))
      case b: BNode => ok(BNodeLabel(b))
      case _ =>
        errStr(s"mkShapeLabel: Node ${n.show} can't be a shape")
    }

  lazy val ignoredPathsClosed: List[Path] = List(Inverse(`sh:targetNode`))

  def mkLabel(label: ShapeMapLabel): ShapeLabel =
    ShapeLabel.fromShapeMapLabel(label)

  def getPaths(se: ShapeExpr, schema: ResolvedSchema): Check[Set[Path]] =
    fromEitherString(se.paths(schema))

  def showCurrentTyping(msg: String, shapesPrefixMap: PrefixMap): Check[Unit] = for {
    typing <- getTyping
    _ <- infoTyping(typing, msg, shapesPrefixMap)
  } yield ()

  def infoTyping(t: ShapeTyping, msg: String, shapesPrefixMap: PrefixMap): Check[Unit] = for {
    nodesPrefixMap <- getNodesPrefixMap
    _ <- info(s"$msg${t.showShort(nodesPrefixMap, shapesPrefixMap)}")
  } yield ()

  def getNodesPrefixMap: Check[PrefixMap] = for {
    rdf <- getRDF
    pm <- fromIO(rdf.getPrefixMap)
  } yield pm

}
