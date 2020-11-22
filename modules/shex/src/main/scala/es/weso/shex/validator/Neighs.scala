package es.weso.shex.validator

import cats._
import cats.implicits._
import es.weso.shex.Path
import es.weso.rdf.nodes.RDFNode
import es.weso.rdf.PrefixMap
import scala.collection.compat.immutable.LazyList
// import es.weso.utils.SetUtils

case class Neighs(m: Map[Path,Set[RDFNode]]) {

  def toList: List[Arc] = {
    m.toList.map { case (p,ns) => ns.toList.map(n => (p,n))}.flatten.map{ case (p,n) => Arc(p,n)} 
  }

  def values(path: Path): Set[RDFNode] = 
   m.getOrElse(path, Set())

  def filterPaths(paths: List[Path]): Neighs = 
   Neighs(m.filterKeys(paths.contains(_)).toMap)

  def showQualified(pm: PrefixMap): String = {
    m.toList.map { case (p,vs) => s"${p.showQualified(pm)} ${vs.map(pm.qualify(_)).mkString(", ")}"}.mkString(s";\n")
  } 

  // TODO: Finish this method and move code from Validator here
  def partition: LazyList[(Neighs,Neighs)] = {
     ??? 
     // SetUtils.pSet(this.toList.toSet).map { case (n1,n2) => (Neighs.fromSet(n1),Neighs.fromSet(n2))} 
  }

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