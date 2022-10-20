package es.weso.wshex
import es.weso.wbmodel._
import es.weso.rbe.interval.IntOrUnbounded

sealed abstract class ListSpec[A]
object ListSpec {
 case class EmptyList[A]() extends ListSpec[A]
 case class EachOf[A](ls: List[ListSpec[A]]) extends ListSpec[A]
 case class OneOf[A](ls: List[ListSpec[A]]) extends ListSpec[A]
 case class Single[A](spec: A) extends ListSpec[A]
}
