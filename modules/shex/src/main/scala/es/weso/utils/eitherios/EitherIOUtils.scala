package es.weso.utils.eitherios

import cats.effect.IO
import cats.data._

object EitherIOUtils {

 def partitionEitherIOS[A,B](vs: List[EitherT[IO,A,B]]): IO[(List[A], List[B])] = {
    val zero: IO[(List[A],List[B])] = IO((List(),List()))
    def cmb(next: IO[(List[A],List[B])], c: EitherT[IO,A,B]): IO[(List[A], List[B])] = for {
      pairs <- next
      e <- c.value
    } yield {
      val (as, bs) = pairs
      e.fold(a => (a::as, bs), b => (as,b::bs))
    }
    vs.foldLeft(zero)(cmb)
 }

 def injectPairLs[A,B,C](v: List[(A,EitherT[IO,B,C])]): List[EitherT[IO,(A,B),(A,C)]] = {
   v.map(pair => {
     val (a,e) = pair
     e.bimap(x => (a,x), y => (a,y))
   })

 }

}