package es.weso.rbe.interval

import es.weso.collection.Bag
import es.weso.rbe.Rbe
import cats._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers


trait BagMatchers extends AnyFunSpec with Matchers with EitherValues {
  def matchBag[A: Show](rbe: Rbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe} should match ${bag}. Open: $open") {
      val checker = IntervalChecker(rbe)
      checker.check(bag, open).isRight should be(true)
    }
  }

  def noMatchBag[A: Show](rbe: Rbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe} should not match ${bag}. Open: $open") {
      val checker = IntervalChecker(rbe)
      checker.check(bag, open).isRight should be(false)
    }
  }

  def equalInterval[A](rbe: Rbe[A], bag: Bag[A], expected: Interval) = {
    it(s"Interval of ${bag} with ${rbe} should be ${expected}") {
      IntervalChecker.interval(rbe, bag).right.value should be(expected)
    }
  }

  def containsBag[A: Show](rbe: Rbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe} should contain ${bag}. Open: $open") {
      val checker = IntervalChecker(rbe)
      checker.check(bag, open).isRight should be(true)
    }
  }

  def notContainsBag[A: Show](rbe: Rbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe} should not contain ${bag}, Open: $open") {
      val checker = IntervalChecker(rbe)
      checker.check(bag, open).isRight should be(false)
    }
  }

}