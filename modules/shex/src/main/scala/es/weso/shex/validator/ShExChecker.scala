package es.weso.shex.validator

import cats._
import cats.data._
import cats.implicits._
// import es.weso.checking.CheckerCats
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.IRI
import es.weso.shex.validator.Action._
import es.weso.shex.validator.Context._
import cats.effect.IO
import fs2.Stream
import es.weso.rdf.nodes.RDFNode
import es.weso.shex.Direct
import es.weso.shex.Inverse
import es.weso.shex.Path
import ValidationUtils._
import es.weso.rdf.triples.RDFTriple
import es.weso.rdf.nodes.BNode
import es.weso.shex.ShapeLabel

object ShExChecker extends CheckerCats {

  type Config = RDFReader
  type Env = Context
  type Err = ShExError
  type Evidence = (NodeShape, String)
  type Log = List[Action]
  
  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    def combine(l1: Log, l2: Log): Log = l1 ++ l2
    def empty: Log = List()
  }

  def errStr[A](msg: String): Check[A] =
    info(s"errorStr($msg)") >> 
    err[A](ShExError.msgErr(msg))

  def fromEitherString[A](e: Either[String,A]): Check[A] =
    fromEither(e.leftMap(ShExError.msgErr(_)))

  def fromStream[A](s: Stream[IO,A]): Check[List[A]] = fromIO(s.compile.toList)

  def fromEitherIOS[A](e: EitherT[IO,String,A]): Check[A] = {
    val ea: Check[Either[String,A]] = EitherT.liftF(WriterT.liftF(ReaderT.liftF(ReaderT.liftF(e.value))))
    for {
      either <- ea
      r <- either.fold(errStr(_), ok)
    } yield r
  }

  def info(msg:String): Check[Unit] = {
    fromIO(
      // IO.pure(())
      IO(println(s"$msg"))
    )
  }

  def checkCond(
                 condition: Boolean,
                 attempt: Attempt,
                 error: ShExError,
                 evidence: String): CheckTyping = for {
    _ <- validateCheck(condition, error)
    newTyping <- addEvidence(attempt.nodeShape, evidence)
  } yield newTyping

  def addEvidence(nodeShape: NodeShape, msg: String): Check[ShapeTyping] = {
    val action = Action(IRI("http://shex.io/actions/log"),Some(s"Evidence added: $nodeShape: $msg"))
    for {
      t <- getTyping
      _ <- addLog(List(action))
    } yield t.addEvidence(nodeShape.node, nodeShape.shape, msg)
  }


  def addNotEvidence(nodeShape: NodeShape, e: ShExError, msg: String): Check[ShapeTyping] = {
    val action = Action(IRI("http://shex.io/actions/log"),Some(s"Not Evidence: $nodeShape: $msg"))
    val node = nodeShape.node
    val shape = nodeShape.shape
    for {
      t <- getTyping
      _ <- addLog(List(action))
    } yield t.addNotEvidence(node, shape, e)
  }

  def runLocalNeighs[A](c: Check[A],
                        node: RDFNode,
                        neighs: Neighs): Check[A] = {
    def liftedF(c: Context): Context = 
       c.addLocalNeighs(node,neighs)
    runLocal(c, liftedF)
  }


  def runLocalTyping[A](c: Check[A],
                        f: ShapeTyping => ShapeTyping): Check[A] = {
    def liftedF(c: Context): Context = c.updateTyping(f)
    runLocal(c, liftedF)
  }

  def bind[A,Other](c1: Check[Other], c2: Check[A]): Check[A] = c1 >> c2 

  def runLocalSafeTyping[A](c: Check[A],
                            f: ShapeTyping => ShapeTyping,
                            safe: (Err, ShapeTyping) => A
                           ): Check[A] = {
    def liftedF(c: Context): Context = c.updateTyping(f)
    def liftedSafe(e: Err, c: Context): A = safe(e,c.typing)
    runLocalSafe(c,liftedF,liftedSafe)
  }


  def getRDF: Check[RDFReader] = getConfig // ask[Comput,RDFReader]

  def getTyping: Check[ShapeTyping] = for {
    env <- getEnv
  } yield env.typing

  def getNeighs(node: RDFNode): Check[Neighs] = for {
    localNeighs <- getLocalNeighs
    neighs <- localNeighs.get(node) match {
      case Some(ns) => ok(ns)
      case None => for {
      rdf        <- getRDF
      outTriples <- fromStream(rdf.triplesWithSubject(node))
      outgoing = outTriples.map(t => Arc(Direct(t.pred), t.obj)).toList
      inTriples <- fromStream(rdf.triplesWithObject(node))
      incoming = inTriples.map(t => Arc(Inverse(t.pred), t.subj)).toList
     } yield {
      val neighs = outgoing ++ incoming
      Neighs.fromList(neighs)
     }
    }
  } yield neighs

  def getVisited: Check[Set[ShapeLabel]] = for {
    context <- getEnv
  } yield context.visited
    

  def getLocalNeighs: Check[LocalNeighs] = for {
    context <- getEnv
  } yield context.localNeighs

  def getNeighPaths(node: RDFNode, paths: List[Path]): Check[Neighs] = 
  {
    val outgoingPredicates = paths.collect { case Direct(p) => p }
    for {
      localNeighs <- getLocalNeighs
      neighs <- localNeighs.get(node) match {
        case Some(ns) => 
         ok(ns.filterPaths(paths))
        case None =>     for {
         rdf        <- getRDF
         outTriples <- fromIO(getTriplesWithSubjectPredicates(rdf,node,outgoingPredicates))
         strRdf <- fromIO(rdf.serialize("TURTLE"))
         outgoing = outTriples.map(t => Arc(Direct(t.pred), t.obj)).toList
        inTriples <- fromStream(rdf.triplesWithObject(node))
        incoming = inTriples.map(t => Arc(Inverse(t.pred), t.subj)).toList
        } yield {
         val neighs = outgoing ++ incoming
         Neighs.fromList(neighs)
        }
      }
    } yield neighs
  }

  def getValuesPath(node: RDFNode, path: Path): Check[Set[RDFNode]] = for {
    localNeighs <- getLocalNeighs
    vs <- localNeighs.get(node) match {
      case Some(ns) => 
       ok(ns.values(path))
      case None =>     for {
       rdf   <- getRDF
       nodes <- fromStream(path.getValues(node, rdf))
      } yield nodes.toSet
    }
  } yield vs

  private def getTriplesWithSubjectPredicates(rdf: RDFReader, 
       node: RDFNode, 
       preds: List[IRI]): IO[List[RDFTriple]] = {
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


  def getNotAllowedPredicates(node: RDFNode, paths: List[Path]): Check[Set[IRI]] =
    for {
      rdf <- getRDF
      ts  <- fromStream(rdf.triplesWithSubject(node))
    } yield {
      val allowedPreds = paths.collect { case Direct(p) => p }
      ts.toSet[RDFTriple].collect {
        case s if !(allowedPreds contains s.pred) => s.pred
      }
    }

  def combineTypings(ts: List[ShapeTyping]): Check[ShapeTyping] = {
    ok(ShapeTyping.combineTypings(ts))
  }


  def combineTypings(ts: ShapeTyping*): Check[ShapeTyping] = {
    ok(ShapeTyping.combineTypings(ts))
  }

  def runCheck[A: Show](
    c: Check[A],
    rdf: RDFReader): IO[CheckResult[ShExError, A, Log]] = {
    val initial: Context = Monoid[Context].empty
    for {
      result <- run(c)(rdf)(initial)
    } yield CheckResult(result)
  }

 

}
