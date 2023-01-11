package es.weso.shex.extend
import cats._
import cats.data._
import cats.implicits._

trait Extend {

  def extendCheckingVisited[S, E: Semigroup, Err, Label](
      s: S,
      finder: Label => Either[Err, S],
      extend: S => Option[List[Label]],
      expr: S => Option[E]
  ): Either[Err, Option[E]] = {
    
    type Visited[A] = State[List[S], A]
    
    def getVisited: Visited[List[S]] = State.get
    
    def addVisited(x: S): Visited[Unit] = {
      State.modify[List[S]](x :: _) 
    }
    
    def ok[A](x: A): Visited[A] = StateT.pure(x)

    def err(e: Err): Visited[Either[Err, Option[E]]] = ok(e.asLeft)

    def combine(e1: Option[E], e2: Option[E]): Option[E] = (e1, e2) match {
      case (None, None)         => None
      case (Some(v1), None)     => Some(v1)
      case (None, Some(v2))     => Some(v2)
      case (Some(v1), Some(v2)) => Some(v1 |+| v2)
    }

    type Result = Either[Err, Option[E]]

    def flattenExprAux(s: S): Visited[Result] = extend(s) match {
      case None => ok(expr(s).asRight)
      case Some(lbls) =>
        def comb(r: Result, x: Label): Visited[Result] =
          for {
            visited <- getVisited
            v <- finder(x).fold(
                  err(_),
                  s1 => 
                    if (visited contains s1) ok(r) // Circular dependency
                    else for {
                      _ <- addVisited(s1)
                      ef <- flattenExprAux(s1)
                    } yield for {
                      v1 <- ef
                      v2 <- r
                    } yield combine(v1, v2))
          } yield v
        Foldable[List].foldM[Visited, Label, Result](lbls, none.asRight)(comb).map(_.map(combine(expr(s),_)))
    }
    val (visited, e) = flattenExprAux(s).run(List(s)).value
    e
  }

  

}
