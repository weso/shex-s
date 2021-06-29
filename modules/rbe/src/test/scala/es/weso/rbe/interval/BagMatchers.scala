package es.weso.rbe.interval

import es.weso.collection.Bag
import es.weso.rbe._
import es.weso.rbe.ShowRbe._
import cats._
import cats.data._
import cats.implicits._
import es.weso.collection.Bag.showBag
import munit._

trait BagMatchers extends FunSuite {
  def matchBag[A: Show](rbe: Rbe[A], bag: Bag[A], open: Boolean = true)(implicit loc: munit.Location): Unit = {
    test(s"${rbe.show} should match ${bag.show}. Open: $open") {
      val checker = IntervalChecker(rbe)
      assertEquals(checker.check(bag, open).isRight, true)
    }
  }

  def noMatchBag[A: Show](rbe: Rbe[A], bag: Bag[A], open: Boolean = true)(implicit loc: munit.Location): Unit = {
    test(s"${rbe.show} should not match ${bag.show}. Open: $open") {
      val checker = IntervalChecker(rbe)
      assertEquals(checker.check(bag, open).isRight, false)
    }
  }

  def equalInterval[A:Show](
    rbe: Rbe[A], bag: Bag[A], expected: Interval
  )(implicit loc: munit.Location) = {
    test(s"Interval of ${bag.show} with ${rbe.show} should be ${expected}") {
      assertEquals(IntervalChecker.interval(rbe, bag), expected.asRight[NonEmptyList[RbeError]])
    }
  }

  def containsBag[A: Show](
    rbe: Rbe[A], bag: Bag[A], open: Boolean = true
    )(implicit loc: munit.Location): Unit = {
    test(s"${rbe.show} should contain ${bag.show}. Open: $open") {
      val checker = IntervalChecker(rbe)
      assertEquals(checker.check(bag, open).isRight, true)
    }
  }

  def notContainsBag[A: Show](rbe: Rbe[A], bag: Bag[A], open: Boolean = true
  )(implicit loc: munit.Location): Unit = {
    test(s"${rbe.show} should not contain ${bag.show}, Open: $open") {
      val checker = IntervalChecker(rbe)
      assertEquals(checker.check(bag, open).isRight, false)
    }
  }

}