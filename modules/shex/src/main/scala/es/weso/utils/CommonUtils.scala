package es.weso.utils

// import es.weso.rdf.nodes.RDFNode
import scala.collection.immutable.Set

object CommonUtils {
  // TODO: This implementation can be replaced by ls.paritionMap(f) in scala 2.13
  def setPartitionMap[A,A1,A2](ls: Set[A])(f: A => Either[A1,A2]): (Set[A1], Set[A2]) = {
    val (left, right) = ls.map(f).partition(_.isLeft)
    (left.map(_.asInstanceOf[Left[A1, _]].value), right.map(_.asInstanceOf[Right[_, A2]].value))
  }

 def mapFun[A,B,C](v: (A,Either[B,C])
                    ): Either[(A,B), (A,C)] = {
    val (node, either) = v
    either match {
      case Left(s) => Left((node,s))
      case Right(s) => Right((node,s))
    }
  }
}