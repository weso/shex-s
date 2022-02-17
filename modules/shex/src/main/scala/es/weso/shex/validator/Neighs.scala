package es.weso.shex.validator

import cats._
import cats.implicits._
import es.weso.shex.Path
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.PrefixMap
import scala.collection.compat.immutable.LazyList
import es.weso.shex.Direct
import es.weso.rdf.nodes.IRI
// import es.weso.utils.SetUtils

case class Neighs(m: Map[Path,Set[RDFNode]]) extends AnyVal {

  def toList: List[Arc] = {
    m.toList.map { case (p,ns) => ns.toList.map(n => (p,n))}.flatten.map{ case (p,n) => Arc(p,n)} 
  }

  def values(path: Path): Set[RDFNode] = 
   m.getOrElse(path, Set())

  def filterPaths(paths: List[Path]): Neighs = 
   Neighs(m.filterKeys(paths.contains(_)).toMap)

  def partitionByPaths(paths: List[Path]): (Neighs,Neighs) = {
    val (m1,m2) = m.partition { case (path,_) => paths.contains(path) }
    (Neighs(m1), Neighs(m2))
  }

  def filterPathCond(cond: Path => Boolean): Neighs =
   Neighs(m.filterKeys(cond(_)).toMap) 

  def showQualified(pm: PrefixMap): String = {
    m.toList.map { case (p,vs) => s"${p.showQualified(pm)} ${vs.map(pm.qualify(_)).mkString(", ")}"}.mkString(s";\n")
  } 

  def getPredicates(): Set[IRI] = 
    m.keySet.collect { case Direct(pred) => pred }

  def nonEmpty: Boolean = m.nonEmpty  
} 

object Neighs {
  def fromSet(s: Set[Arc]): Neighs = 
    fromList(s.toList)

  def fromList(ls: List[Arc]): Neighs = {
      val zero: Map[Path,Set[RDFNode]] = Map()
      def cmb(current: Map[Path,Set[RDFNode]], x: Arc): Map[Path,Set[RDFNode]] = {
        val p = x.path
        val n = x.node  
        current.get(p) match {
          case None => current.updated(p,Set(n))
          case Some(s) => current.updated(p, s + n)
        }
      }
      Neighs(ls.foldLeft(zero)(cmb))
  }

 implicit val neighsMonoid: Monoid[Neighs] = new Monoid[Neighs] {
    def combine(n1: Neighs, n2: Neighs): Neighs = 
     Neighs(n1.m |+| n2.m)

    def empty: Neighs = Neighs(Monoid[Map[Path,Set[RDFNode]]].empty)
  } 
}