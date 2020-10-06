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

object ShExChecker extends CheckerCats {

  type Config = RDFReader
  type Env = Context
  type Err = ShExError
  type Evidence = (NodeShape, String)
  type Log = List[Action]
  type CheckTyping = Check[ShapeTyping]

  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    def combine(l1: Log, l2: Log): Log = l1 ++ l2
    def empty: Log = List()
  }

  def errStr[A](msg: String): Check[A] =
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
       IO.pure(())
      // IO(println(s"$msg"))
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

  def runLocalTyping[A](c: Check[A],
                        f: ShapeTyping => ShapeTyping): Check[A] = {
    def liftedF(c: Context): Context = Context.updateTyping(c,f)
    runLocal(c, liftedF)
  }

  def bind[A,Other](c1: Check[Other], c2: Check[A]): Check[A] = c1 >> c2 /* for {
    _ <- c1
    v <- c2
  } yield v */

  def runLocalSafeTyping[A](c: Check[A],
                            f: ShapeTyping => ShapeTyping,
                            safe: (Err, ShapeTyping) => A
                           ): Check[A] = {
    def liftedF(c: Context): Context = Context.updateTyping(c,f)
    def liftedSafe(e: Err, c: Context): A = safe(e,c.typing)
    runLocalSafe(c,liftedF,liftedSafe)
  }


  def getRDF: Check[RDFReader] = getConfig // ask[Comput,RDFReader]

  def getTyping: Check[ShapeTyping] = for {
    env <- getEnv
  } yield env.typing

  def combineTypings(ts: Seq[ShapeTyping]): Check[ShapeTyping] = {
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

/*  def runCheckWithTyping[A: Show](
    c: Check[A],
    rdf: RDFReader,
    typing: ShapeTyping): CheckResult[ShExError, A, Log] = {
    val ctx = Context.fromTyping(typing)
    val r = run(c)(rdf)(ctx)
    CheckResult(r)
  } */

}
