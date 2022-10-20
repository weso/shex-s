package es.weso.rbe.deriv

import es.weso.rbe._
import munit._
import es.weso.collection._
import cats._
import cats.implicits._

class DerivTest extends FunSuite {

  shouldCheck(Or(Symbol("a",0,1), Symbol("b",0,1)),Bag("a"), Bag("a"), true)
  shouldCheck(Or(Symbol("a",0,1), Symbol("b",0,1)),Bag("b"), Bag("b"), true)
  shouldNotCheck(Or(Symbol("a",0,1), Symbol("b",0,1)), Bag("a","b"), true)
  shouldNotCheck(Or(Symbol("a",0,1), Symbol("b",0,1)), Bag("a","b"), false)
  shouldCheck(And(Symbol("a",0,1), Symbol("b",0,1)), Bag("a","b"), Bag("a","b"), true)
  shouldCheck(Symbol("a",0,1), Bag("a"), Bag("a"), true)
  shouldCheck(Symbol("a",0,1), Bag("b"), Bag("b"), true)  // should expect: Bag[String]()
  shouldNotCheck(Symbol("a",0,1), Bag("b"), false)
  shouldNotCheck(Symbol("a",0,1), Bag("a","b"), false)
  shouldNotCheck(Symbol("a",0,1), Bag("a","a"), true)
  shouldNotCheck(Symbol("a",1,1), Bag("a","a"), true)
  shouldNotCheck(Symbol("a",1,1), Bag("a","a"), false)
  shouldCheck(Symbol("a",1,1), Bag("a","b"), Bag("a", "b"), true)  // should expect: Bag("a")
  shouldNotCheck(Symbol("a",1,1), Bag("a","b"), false)
  
  def shouldCheck[A: Show](
    rbe: Rbe[A], 
    bag: Bag[A], 
    expected: Bag[A],
    open: Boolean = true,
    )(implicit loc: munit.Location): Unit = {
    test(s"$rbe ~  $bag ($open)") {
      val derivMatcher = DerivChecker(rbe)
      derivMatcher.check(bag,open).fold(
        err => fail(s"Errors matching: $err"),
        b => assertEquals(b, expected)
      )
    }
  }
  
  def shouldNotCheck[A: Show](
    rbe: Rbe[A], 
    bag: Bag[A], 
    open: Boolean = true
    )(implicit loc: munit.Location): Unit = {
    test(s"$rbe !~ $bag ($open)") {
      val derivMatcher = DerivChecker(rbe)
      derivMatcher.check(bag,open).fold(
        err => (),
        b => fail(s"Should fail but passed with $b")
      )
    }
  }

}