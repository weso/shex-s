package es.weso.shex.validator
import scala.collection.compat.immutable.LazyList
import es.weso.utils.SetUtils._

object PartitionUtils {

  trait Entry[A,B] {
    val key: A
    val value: B
  }

  case class Available[A](values: Set[A]) extends AnyVal {
      def contains(x: A): Boolean = values.contains(x)
  }
  
  case class ResultLine[A,B](ls: List[Set[Entry[A,B]]]) extends AnyVal {
      def add(s: Set[Entry[A,B]]): ResultLine[A,B] = ResultLine(ls :+ s)
      def addParts(e: Entry[A,B], ps: Set[Entry[A,B]]): LazyList[ResultLine[A,B]] = 
          ??? // intercalate(e,ps)
  }

  def intercalate[A](e: A, s: Set[A]): LazyList[Set[A]] = ???

  object ResultLine {
      def empty[A,B](n: Int): ResultLine[A,B] = 
          ResultLine(List.fill(n)(Set[Entry[A,B]]()))
  }

  def partsOver[A,B](
    ns: List[Entry[A,B]], 
    ps: List[Available[A]]): LazyList[ResultLine[A,B]] = ns match {
        case Nil => LazyList(ResultLine.empty(ps.length)) 
        case pair :: pairs => partsOver(pairs, ps).map(addPair(pair, ps)).flatten
    }

  def addPair[A,B](
    elem: Entry[A,B], 
    ps: List[Available[A]])(
    current: ResultLine[A,B]
    ): LazyList[ResultLine[A,B]] = {
        val pairs: List[(Available[A], Set[Entry[A,B]])] = ps.zip(current.ls)
        val zero = LazyList[ResultLine[A,B]]()
        pairs.foldRight(zero) {
            case (pair, rest) => {
                val (allowed, existing) = pair
                if (allowed contains elem.key) rest.flatMap(_.addParts(elem, existing))
                else rest.map(_.add(existing))
            }
        }
    }

}