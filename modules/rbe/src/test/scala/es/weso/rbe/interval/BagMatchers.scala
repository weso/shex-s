package es.weso.rbe.interval

import es.weso.collection.Bag
import es.weso.rbe.Rbe
import es.weso.rbe.ShowRbe._
import cats._
import cats.implicits._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import es.weso.collection.Bag.showBag

trait BagMatchers extends AnyFunSpec with Matchers with EitherValues {
  def matchBag[A: Show](rbe: Rbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe.show} should match ${bag.show}. Open: $open") {
      val checker = IntervalChecker(rbe)
      checker.check(bag, open).isRight should be(true)
    }
  }

  def noMatchBag[A: Show](rbe: Rbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe.show} should not match ${bag.show}. Open: $open") {
      val checker = IntervalChecker(rbe)
      checker.check(bag, open).isRight should be(false)
    }
  }

  def equalInterval[A:Show](rbe: Rbe[A], bag: Bag[A], expected: Interval) = {
    it(s"Interval of ${bag.show} with ${rbe.show} should be ${expected}") {
      IntervalChecker.interval(rbe, bag).right.value should be(expected)
    }
  }

  def containsBag[A: Show](rbe: Rbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe.show} should contain ${bag.show}. Open: $open") {
      val checker = IntervalChecker(rbe)
      checker.check(bag, open).isRight should be(true)
    }
  }

  def notContainsBag[A: Show](rbe: Rbe[A], bag: Bag[A], open: Boolean = true) = {
    it(s"${rbe.show} should not contain ${bag.show}, Open: $open") {
      val checker = IntervalChecker(rbe)
      checker.check(bag, open).isRight should be(false)
    }
  }

}