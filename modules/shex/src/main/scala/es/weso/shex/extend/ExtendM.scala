package es.weso.shex.extend
import cats._
import cats.data._
import cats.implicits._

trait ExtendM {

  /* Same as extendCheckingVisited but it does a different computation to the first value than the rest */
  def extendCheckingVisitedM1[S, Result: Monoid, Label, M[_]: Monad](
      s: S,
      finder: Label => M[S],
      extend: S => M[List[Label]],
      first: S => M[Result], 
      rest: S => M[Result], 
  ): M[Result] = {

    type Visited = List[S]
    type Cmp[A] = StateT[M, Visited, A]
    
    def getVisited: Cmp[Visited] = StateT.get[M, Visited]
    def addVisited(x: S): Cmp[Unit] = StateT.modify[M, List[S]](x :: _) 
    def ok[A](x: A): Cmp[A] = StateT.pure(x)
    def lift[A](x: M[A]): Cmp[A] = StateT.liftF(x)

    def comb(current: Result, lbl: Label): Cmp[Result] = {
      getVisited.flatMap(visited => 
        lift(finder(lbl)).flatMap(found => 
          if (visited contains found) ok(current)
          else addVisited(found).flatMap(_ => 
                lift(rest(found)).flatMap(resultFound => 
                  lift(extend(found)).flatMap(extendedLabels => 
                    extendLabels(extendedLabels).flatMap(restResult => 
                      ok(current |+| resultFound |+| restResult))
                  )))))
    }

    def extendLabels(lbls: List[Label]): Cmp[Result] = {
      Foldable[List].foldM[Cmp, Label, Result](lbls, Monoid[Result].empty)(comb)
    }

    lift(first(s)).flatMap(firstResult => 
      lift(extend(s)).flatMap(lbls => extendLabels(lbls).flatMap(rest => ok(firstResult |+| rest))
        )).run(List(s)).map(_._2)

  }

  def extendCheckingVisitedM[S, Result: Monoid, Label, M[_]: Monad](
      s: S,
      finder: Label => M[S],
      extend: S => M[List[Label]],
      expr: S => M[Result]
  ): M[Result] = {
    extendCheckingVisitedM1(s, finder, extend, expr, expr)
    
/*    type Visited = List[S]
    type Cmp[A] = StateT[M, Visited, A]
    
    def getVisited: Cmp[Visited] = StateT.get[M, Visited]
    
    def addVisited(x: S): Cmp[Unit] = {
      StateT.modify[M, List[S]](x :: _) 
    }
    
    def ok[A](x: A): Cmp[A] = StateT.pure(x)

    def lift[A](x: M[A]): Cmp[A] = StateT.liftF(x)

    def comb(current: Result, lbl: Label): Cmp[Result] = {
      getVisited.flatMap(visited => 
        lift(finder(lbl)).flatMap(found => 
          if (visited contains found) ok(current)
          else addVisited(found).flatMap(_ => 
                extendAux(found).flatMap(rest => 
                 ok(current |+| rest))
          )))
    }

    def extendAux(s: S): Cmp[Result] = {
      lift(expr(s)).flatMap(r => 
       lift(extend(s)).flatMap(lbls =>
        Foldable[List].foldM[Cmp, Label, Result](lbls, Monoid[Result].empty)(comb))
        .map(r |+| _ )
      )  
    }

    extendAux(s).run(List(s)).map(_._2)
*/
 }  

}
