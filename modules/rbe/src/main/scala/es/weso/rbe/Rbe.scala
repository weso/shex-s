package es.weso.rbe

import es.weso.collection._
import interval._
import IntOrUnbounded._
import cats._
import cats.implicits._

/**
 * This trait defines Single Occurrence Regular Bag Expressions (Rbe)
 *
 * == Further info ==
 *
 * The algorithm to check that a Rbe contains a bag is PTIME
 * The algorithm has been described in [1] and is based on intervals
 *
 * [1] Complexity and Expressiveness of ShEx for RDF,
 *     S. Staworko, I. Boneva, J. Labra, S. Hym, E. Prud'hommeaux, H. Solbrig
 *
 */
sealed trait Rbe[+A] extends Product with Serializable {

  /**
   * Checks if a RBE contains repetitions
   */
  lazy val containsRepeats: Boolean = {
    this match {
      case _: Fail => false
      case Empty => false
      case Symbol(_, _, _) => false
      case And(e1, e2) => e1.containsRepeats || e2.containsRepeats
      case Or(e1, e2) => e1.containsRepeats || e2.containsRepeats
      case Star(e) => e.containsRepeats
      case Plus(e) => e.containsRepeats
      case Repeat(_, _, _) => true
    }
  }

  /**
   * Symbols that contains this rbe
   *
   * Example: {{{
   * symbols(Or(And(Symbol("a",1,3),Symbol("b",1,1)),Symbol("b",2,3))) == Seq("a","b")
   * }}}
   */
  lazy val symbols: Seq[A] = {
    this match {
      case _ : Fail => List()
      case Empty => List()
      case Symbol(a, _, _) => List(a)
      case And(v1, v2) => v1.symbols ++ v2.symbols // v1.symbols concat v2.symbols
      case Or(v1, v2) => v1.symbols ++ v2.symbols
      case Star(v) => v.symbols
      case Plus(v) => v.symbols
      case Repeat(v, _, _) => v.symbols
    }
  }

  /**
   * Checks that there are no symbols in common with a bag
   */
  private[rbe] def noSymbolsInBag[U >: A](bag: Bag[U]): Boolean = {
    this.symbols.forall(x => bag.multiplicity(x) == 0)
  }

  /**
   * Derivative over a bag of symbols
   * @param open allows extra symbols
   * @param controlled limits the extra symbols to those that don't appear in controlled
   */
  def derivBag[U >: A](bag: Bag[U], open: Boolean, controlled: Seq[U])(implicit r: Show[U]): Rbe[U] = {
    val e: Rbe[U] = this
    def f(x: U, rest: Rbe[U]): Rbe[U] = {
      val v: Rbe[U] = rest.deriv(x, open, controlled)(r)
      v
    }
    bag.toSeq.foldRight(e)(f)
  }

  /**
   * Checks if a rbe is nullable
   */
  def nullable[U >: A]: Either[Map[U,Int],Unit]  = {
    this match {
      case _: Fail => Left(Map())
      case Empty => Right(())
      case Symbol(_,0,_) => Right(())
      case Symbol(x,m,n) => Left(Map(x -> m))
      case And(e1,e2) => combineAnd(e1.nullable, e2.nullable)
      case Or(e1,e2) => combineOr(e1.nullable, e2.nullable)
      case Star(_) => Right(())
      case Plus(e) => e.nullable
      case Repeat(_,0,_) => Right(())
      case Repeat(e, _, _) => e.nullable 
    }
  }

  private def combineAnd[B](
    s1: Either[Map[B,Int],Unit], 
    s2: Either[Map[B,Int], Unit]
    ): Either[Map[B,Int],Unit] = (s1,s2) match {
      case (Left(m), Right(_)) => Left(m)
      case (Right(_), Left(m)) => Left(m)
      case (Right(_), Right(_)) => Right(())
      case (Left(m1), Left(m2)) => Left(m1 |+| m2)
    }

  private def combineOr[B](
    s1: Either[Map[B,Int],Unit], 
    s2: Either[Map[B,Int], Unit]
    ): Either[Map[B,Int],Unit] = (s1,s2) match {
      case (Right(_), Left(_)) => Right(())
      case (Right(_), Right(_)) => Right(())
      case (Left(m1), Left(m2)) => Left(m1 |+| m2)
      case (Left(_), Right(_)) => Right(())
    }


  private def mkAnd[U >: A](r1: => Rbe[U], r2: => Rbe[U]): Rbe[U] = {
    val r = (r1, r2) match {
      case (Empty, e2) => e2
      case (e1, Empty) => e1
      case (f @ Fail(_), _) => f
      case (_, f @ Fail(_)) => f
      case (_, _) => And(r1, r2)
    }
    r
  }

  private def mkRange[U >: A](e: Rbe[U], m: Int, n: IntOrUnbounded): Rbe[U] = {
    if (m < 0) Fail(RangeNegativeLowerBound(m))
    else if (m > n) Fail(RangeLowerBoundBigger(m,n))
    else {
      (m, n, e) match {
        case (0, IntLimit(0), _) => Empty
        case (1, IntLimit(1), e) => e
        case (_, _, f @ Fail(_)) => f
        case (_, _, Empty) => Empty
        case (m, n, e) => Repeat(e, m, n)
      }
    }
  }

  private def mkRangeSymbol[U >: A](x: U, m: Int, n: IntOrUnbounded): Rbe[U] = {
    if (m < 0) Fail(RangeNegativeLowerBound(m))
    else if (m > n) Fail(RangeLowerBoundBigger(m,n))
    else {
      (m, n) match {
        //        case (0, IntLimit(0)) => Empty
        //        case (1, IntLimit(1)) => Empty
        case (m, n) => Symbol(x, m, n)
      }
    }
  }

  private def mkOr[U >: A](r1: => Rbe[U], r2: => Rbe[U]): Rbe[U] = {
    val r = (r1, r2) match {
      case (Fail(_), e2) => e2
      case (e1, Fail(_)) => e1
      case (e1, e2) =>
        if (e1 == e2) e1
        else Or(e1, e2)
    }
    r
  }

/*  private def mkRepeat[U >: A](r: => Rbe[U], m: Int, n: IntOrUnbounded): Rbe[U] = {
    Repeat(r, m, n)
  } */

  private def derivSymbol[U >: A](x: U, s: Symbol[U], open: Boolean, controlled: Seq[U])(implicit r: Show[U]): Rbe[U] = {
    if (x == s.a) {
      if (s.m == IntLimit(0))
        Fail(MaxCardinalityZeroFoundValue(x,s))
      else
        mkRangeSymbol(s.a, math.max(s.n - 1, 0), s.m.minusOne)
    } else if (open && !(controlled contains x)) {
      this
    } else {
      Fail(Unexpected(x,s,open))
    }
  }

  /**
   * derivative of this RBE with regards to a symbol
   * @param x symbol
   * @param open allows extra symbols
   * @param controlled defines the symbols that are allowed in closed expressions
   */
  def deriv[U >: A](x: U, open: Boolean, controlled: Seq[U])(implicit r: Show[U]): Rbe[U] = {
    this match {
      case f @ Fail(_) => f
      case Empty =>
        if (open && !(controlled contains x))
          Empty
        else
          Fail(UnexpectedEmpty(x,open)(r))
      case s @ Symbol(_, _, _) => {
        derivSymbol(x, s, open, controlled)
      }
      case And(e1, e2) => {
        lazy val d1 = e1.deriv(x, open, controlled)
        lazy val d2 = e2.deriv(x, open, controlled)
        mkOr(mkAnd(d1, e2), mkAnd(d2, e1))
      }
      case Or(e1, e2) => {
        lazy val d1 = e1.deriv(x, open, controlled)
        lazy val d2 = e2.deriv(x, open, controlled)
        mkOr(d1, d2)
      }
      case Star(e) => {
        val d = e.deriv(x, open, controlled)
        mkAnd(d, e)
      }
      case Plus(e) => {
        val d = e.deriv(x, open, controlled)
        mkAnd(d, Star(e))
      }
      case Repeat(e, 0, IntLimit(0)) => {
        val d = e.deriv(x, open, controlled)
        if (d.nullable.isRight) Fail(CardinalityZeroZeroDeriv(x,e,d))
        else Empty
      }
      case Repeat(e, m, n) => {
        lazy val d: Rbe[U] = e.deriv(x, open, controlled)(r)
        // println(s"Repeat: deriv of $e/$x = $d")
        lazy val rest = mkRange(e, math.max(m - 1, 0), n.minusOne)
        // println(s"Repeat: rest $rest")
        val v = mkAnd(d, rest)
        // println(s"Repeat: and: $r")
        v
      }
    }
  }

}

/**
 * Fail RBE doesn't match
 */
case class Fail(error: RbeError) extends Rbe[Nothing]

/**
 * Empty RBE
 */
case object Empty extends Rbe[Nothing]

/**
 * Represents a symbol that is repeated between n and m times (m can be unbounded)
 */
case class Symbol[+A](a: A, n: Int, m: IntOrUnbounded) extends Rbe[A]

/**
 * And(v1,v2) represents both v1 and v2
 */
case class And[A](v1: Rbe[A], v2: Rbe[A]) extends Rbe[A]

/**
 * Or(v1,v2) represents either v1 or v2
 */
case class Or[A](v1: Rbe[A], v2: Rbe[A]) extends Rbe[A]

/**
 * Star(v) represents 0 or more v
 */
case class Star[A](v: Rbe[A]) extends Rbe[A]

/**
 * Plus(v) represents 1 or more appearances of v
 */
case class Plus[A](v: Rbe[A]) extends Rbe[A]

/**
 * Repeat(v,n,m) represents between n and m apperances of v
 */
case class Repeat[A](v: Rbe[A], n: Int, m: IntOrUnbounded) extends Rbe[A]


object Rbe {

  def show[A:Show](r: Rbe[A]): String = {
    import ShowRbe._
    Show[Rbe[A]].show(r)
  }


}
