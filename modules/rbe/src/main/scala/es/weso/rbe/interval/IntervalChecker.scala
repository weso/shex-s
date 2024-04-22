package es.weso.rbe.interval

import es.weso.collection._
import es.weso.rbe._
import es.weso.rbe.deriv._
import es.weso.rbe.interval.IntOrUnbounded._
import cats._
import cats.data._
import cats.implicits._

case class IntervalChecker[A: Show](rbe: Rbe[A]) extends BagChecker[A] {

  type Matched[B] = Either[NonEmptyList[RbeError], B]

  def isOk[B](m: Matched[B]): Boolean = m.isRight

  lazy val derivChecker = DerivChecker(rbe)

  def check(bag: Bag[A], open: Boolean): Matched[Bag[A]] =
    if (rbe.containsRepeats) {
      // TODO: Check if Repeat(X,0,0) could be treated specially
      derivChecker.check(bag, open)
    } else {
      if (!open && extraSymbols(bag).isEmpty == false)
        Left(
          NonEmptyList.one(
            MsgError(
              s"$rbe doesn't match bag $bag. Open: $open, Extra symbols: ${extraSymbols(bag)}"
            )
          )
        )
      else
        for {
          interval <- IntervalChecker.interval(rbe, bag)
          v <-
            if (interval.contains(1))
              bag.asRight
            else
              // In case of fail, check using derivatives to obtain better error messages
              // TODO: Could it be optimized knowing that it will fail?
              derivChecker.check(bag, open)
          // NonEmptyList.one(IntervalError(interval,rbe,bag,open)).asLeft
        } yield v
    }

  private def extraSymbols(bag: Bag[A]): Seq[A] =
    bag.elems.map(_._1).filter(!rbe.symbols.contains(_)).toSeq

}

object IntervalChecker {

  def interval[A: Show](rbe: Rbe[A], bag: Bag[A]): Either[NonEmptyList[RbeError], Interval] =
    // println(s"Interval of $rbe with $bag")
    rbe match {
      case Fail(_) => Interval(1, 0).asRight
      case Empty   => Interval(0, Unbounded).asRight
      case Symbol(a, n, m) =>
        val wa = bag.multiplicity(a)
        Interval(divIntLimitUp(wa, m), divIntLimitDown(wa, n)).asRight
      case And(v1, v2) =>
        for {
          i1 <- interval(v1, bag)
          i2 <- interval(v2, bag)
        } yield i1 & i2
      case Or(v1, v2) =>
        for {
          i1 <- interval(v1, bag)
          i2 <- interval(v2, bag)
        } yield i1 + i2
      case Star(v) =>
        if (rbe.noSymbolsInBag(bag)) Interval(0, Unbounded).asRight
        else
          for {
            ie <- interval(v, bag)
            v =
              if (ie.isEmpty) ie
              else Interval(1, Unbounded)
          } yield v
      case Plus(v) =>
        if (rbe.noSymbolsInBag(bag)) Interval(0, 0).asRight
        else
          for {
            ie <- interval(v, bag)
            v =
              if (ie.isEmpty) ie
              else Interval(1, ie.m)
          } yield v

      // Having repetitions on expressions breaks the single-occurrence bag expression
      // This case is handled by detecting repetitions and invoking the derivatives algorithm
      case r @ Repeat(_, _, _) =>
        NonEmptyList.one(RepeatsError(r, rbe, bag)).asLeft

    }

}
