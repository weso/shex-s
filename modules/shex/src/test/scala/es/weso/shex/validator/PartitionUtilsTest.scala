package es.weso.shex.validator

import munit._
import cats._
import scala.collection.compat.immutable.LazyList
import PartitionUtils._

class PartitionUtilsTest extends FunSuite {

  case class E(key: String, value: Int) extends Entry[String, Int]

  {
    val ns: Set[Entry[String, Int]] = Set(E("p", 1), E("p", 2))
    val ls: List[Available[String]] =
      List(Available(Set("p")))
    val expected: LazyList[List[Set[Entry[String, Int]]]] =
      LazyList(List(Set(E("p", 1), E("p", 2))))
    testPartsOver(ns, ls, expected)
  }

  {
    val ns: Set[Entry[String, Int]] = Set(E("p", 1), E("p", 2))
    val ls: List[Available[String]] = List(Available(Set("q")))
    val expected: LazyList[List[Set[Entry[String, Int]]]] = LazyList()
    testPartsOver(ns, ls, expected)
  }

  {
    val set: Set[Entry[String, Int]] = Set(E("p", 1), E("q", 1))
    val ps: List[Available[String]] = List(Available(Set("q")))
    val expected: LazyList[List[Set[Entry[String, Int]]]] = LazyList()
    testPartsOver(set, ps, expected)
  }

  {
    val ns: Set[Entry[String, Int]] = Set(E("p", 1), E("q", 1))
    val ls: List[Available[String]] = List(Available(Set("p")), Available(Set("q")))
    val expected: LazyList[List[Set[Entry[String, Int]]]] = LazyList(
      List(Set(E("p", 1)), Set(E("q", 1)))
    )
    testPartsOver(ns, ls, expected)
  }

  {
    val ns: Set[Entry[String, Int]] = Set(E("p", 1), E("q", 1))
    val ls: List[Available[String]] =
      List(Available(Set("p")), Available(Set()), Available(Set("q")))
    val expected: LazyList[List[Set[Entry[String, Int]]]] = LazyList(
      List(Set(E("p", 1)), Set(), Set(E("q", 1)))
    )
    testPartsOver(ns, ls, expected)
  }

  {
    val ns: Set[Entry[String, Int]] = Set(E("p", 1), E("p", 2))
    val ls: List[Available[String]] =
      List(Available(Set("p")), Available(Set()), Available(Set("p")))
    val expected: LazyList[List[Set[Entry[String, Int]]]] = LazyList(
      List(Set(E("p", 1), E("p", 2)), Set(), Set()),
      List(Set(E("p", 2)), Set(), Set(E("p", 1))),
      List(Set(E("p", 1)), Set(), Set(E("p", 2))),
      List(Set(), Set(), Set(E("p", 1), E("p", 2)))
    )
    testPartsOver(ns, ls, expected)
  }

  {
    val ns: Set[Entry[String, Int]] = Set(E("p", 1), E("p", 2), E("q", 1))
    val ls: List[Available[String]] = List(Available(Set("p", "q")), Available(Set("p")))
    val expected: LazyList[List[Set[Entry[String, Int]]]] = LazyList(
      List(Set(E("p", 1), E("p", 2), E("q", 1)), Set()),
      List(Set(E("p", 2), E("q", 1)), Set(E("p", 1))),
      List(Set(E("p", 1), E("q", 1)), Set(E("p", 2))),
      List(Set(E("q", 1)), Set(E("p", 1), E("p", 2)))
    )
    testPartsOver(ns, ls, expected)
  }

  {
    val ns: Set[Entry[String, Int]] = Set(E("p", 1), E("p", 2))
    val ls: List[Available[String]] = List(Available(Set()), Available(Set("p")))
    val expected: LazyList[List[Set[Entry[String, Int]]]] = LazyList(
      List(Set(), Set(E("p",1), E("p",2)))
    )
    testPartsOver(ns, ls, expected)
  }


  def testPartsOver(
      set: Set[Entry[String, Int]],
      availables: List[Available[String]],
      expected: LazyList[List[Set[Entry[String, Int]]]]
  )(implicit loc: munit.Location): Unit =
    test(s"Set: ${set}, available: ${availables}") {
      val result = PartitionUtils.partsOver(set, availables)
      assertEquals(result.toList, expected.toList)
    }
}
