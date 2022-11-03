package es.weso.shex.validator
import es.weso.utils.internal.CollectionCompat._
import es.weso.utils.SetUtils._

object PartitionUtils {

  trait Entry[A, B] { 
    val key: A  
    val value: B
  }

  case class Available[A](values: Set[A]) extends AnyVal {

    def contains[B](x: Entry[A, B]): Boolean =
      values.filter(v => v == x.key).nonEmpty

    def containsAll[B](x: Set[Entry[A, B]]): Boolean =
      x.forall(contains(_))
  }

  def partsOver[A, B](
      set: Set[Entry[A, B]],
      ps: List[Available[A]]
  ): LazyList[List[Set[Entry[A, B]]]] =
    partition(set, ps.length).filter(line => matchLine(ps, line))

  def matchLine[A, B](ps: List[Available[A]], line: List[Set[Entry[A, B]]]): Boolean =
    ps.zip(line).forall { case (a, set) => a.containsAll(set) }

  def allowed[A, B](s: Set[Entry[A, B]], available: Available[A]): Boolean =
    s.forall(e => available.contains(e))

}
