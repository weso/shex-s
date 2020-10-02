package es.weso.shex.spec
import cats._
import cats.data._
import cats.syntax.all._
import es.weso.rdf.RDFReader
import es.weso.rdf.nodes.RDFNode
import es.weso.shapeMaps._
import es.weso.shex.Schema
import es.weso.utils.internal.CollectionCompat._
import cats.effect._
import fs2.Stream

object Check {

  type Evidence = String
  type ShapeTyping = TypingMap[RDFNode, ShapeMapLabel, Evidence]
  type ReaderEnv[A] = ReaderT[IO, Env, A]
  type Check[A] = EitherT[ReaderEnv,String,A]

  def emptyTyping: ShapeTyping = TypingMap.empty

  def fromEither[A](e: Either[String, A]): Check[A] =
    EitherT.fromEither[ReaderEnv](e)

  def fromIO[A](ioa: IO[A]): Check[A] = 
    EitherT.liftF(ReaderT.liftF(ioa))

  def fromStream[A](s: Stream[IO,A]): Check[List[A]] =
    fromIO(s.compile.toList)

  def satisfyChain[A](ls: List[A], check: A => Check[ShapeTyping]): Check[ShapeTyping] = {
    val zero: Check[ShapeTyping] = getTyping
    def cmb(next: Check[ShapeTyping],x: A): Check[ShapeTyping] = for {
      typing <- check(x)
      newTyping <- runLocalWithTyping(_ => Right(typing), next)
    } yield newTyping
    ls.foldLeft(zero)(cmb)
  }

  def optSatisfy[A](maybeA: Option[A], check: A => Check[Boolean]): Check[Boolean] =
   maybeA match {
    case None => true.pure[Check]
    case Some(a) => check(a)
  }

  def satisfyOr(c1: Check[Boolean],
                c2: => Check[Boolean]
               ): Check[Boolean] = c1.flatMap(x =>
    if (x) pure(true) else c2
  )

  def satisfyAnd(c1: Check[Boolean],
                 c2: => Check[Boolean]
                ): Check[Boolean] = c1.flatMap(x =>
    if (x) c2 else pure(false)
  )

  def sequence[A](ls: List[Check[A]]): Check[List[A]] =
    ls.sequence

  def satisfyAll(ls: List[Check[Boolean]]): Check[Boolean] =
    ls.sequence.map(_.forall(_ == true))

  def satisfySome(ls: List[Check[Boolean]]): Check[Boolean] =
    ls.sequence.map(_.exists(_ == true))

  def satisfyNot(check: Check[Boolean]): Check[Boolean] =
    check.map(e => !e)

  def satisfyFirst[A,F[_]: Monad](ls: => LazyList[A],
                                  check: A => F[Boolean]
                                 ): F[Boolean] = {
    val z : Eval[F[Boolean]] = Eval.later(Monad[F].pure(false))
    def cmb(x : A, next: Eval[F[Boolean]]): Eval[F[Boolean]] =
      Eval.later(
        for {
         b <- check(x)
         n <- if (b) Monad[F].pure(true)
              else next.value
        } yield n
      )
    Foldable[LazyList].foldRight(ls,z)(cmb).value
    // I want to do the following but it gives an error
    // Foldable[LazyList].foldRight(ls,z)(cmb).value
  }


  def unimplemented[A](msg: String): Check[A] =
    err(s"Unimplemented: $msg")

  def pure[A](x:A): Check[A] = x.pure[Check]

  def err[A](msg: String): Check[A] = EitherT.leftT[ReaderEnv,A](msg)

  def getSchema: Check[Schema] = {
    val r: ReaderEnv[Env] = ReaderT.ask
    for {
      env <- EitherT.liftF[ReaderEnv,String,Env](r)
    } yield env.schema
  }

  def getTyping: Check[ShapeTyping] = {
    val r: ReaderEnv[Env] = ReaderT.ask
    for {
      env <- EitherT.liftF[ReaderEnv,String,Env](r)
    } yield env.typing
  }

  def getRDF: Check[RDFReader] = {
    val r: ReaderEnv[Env] = ReaderT.ask
    for {
      env <- EitherT.liftF[ReaderEnv,String,Env](r)
    } yield env.rdf
  }

  def runLocal[A](fn: Env => Env, check: Check[A]): Check[A] = {
    EitherT(check.value.local(fn))
  }

  def runLocalWithTyping[A](fn: ShapeTyping => Either[String,ShapeTyping], check: Check[A]): Check[A] = for {
    typing <- getTyping
    newTyping <- fn(typing).fold(e => err(e), t => pure(t))
    x <- runLocal(env => env.copy(typing = newTyping), check)
  } yield x

  def runCheck[A](env: Env, check: Check[A]): IO[A] = for {
    ex <- check.value.run(env)
    x <- ex.fold(
      e => IO.raiseError(new RuntimeException(s"Error: $e")),
      IO(_)
    )
  } yield x

}

