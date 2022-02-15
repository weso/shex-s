package es.weso.shex.validator

import munit._
import cats._
import scala.collection.compat.immutable.LazyList
import PartitionUtils._

class PartitionUtilsTest extends FunSuite {

  
 test("partsOver") {

  case class E(key: String, value: Int) extends Entry[String,Int]  

  val ls: List[Available[String]] = List(Available(Set("p","q")), Available(Set()), Available(Set("p")), Available(Set("q")))
  val ns: List[E] = List(E("p",1),E("p",2),E("q",1))
  val expected: LazyList[ResultLine[String,Int]] = ???
      /* LazyList(
         List(Set(("p",1), ("p",2),("q",1)), Set(), Set(), Set()),
         List(Set(("p",1), ("p",2)), Set(), Set(), Set(("q",1))),
         List(Set(("p",1), ("q",1)), Set(), Set(("p",2)), Set()),
         List(Set(("p",1), ("q",1)), Set(), Set(("p",2)), Set())
         // ...
      ) */
  assertEquals(PartitionUtils.partsOver(ns,ls), expected)
 }

}
