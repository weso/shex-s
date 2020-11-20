package es.weso.depgraphs
import cats.effect._

trait Inheritance[Node] {

  def clear: IO[Unit]

  def nodes: IO[Set[Node]]

  def addNode(node: Node): IO[Unit]

  def addInheritance(node1: Node, node2: Node): IO[Unit]

  def descendants(node: Node): IO[Set[Node]]

  def ancestors(node: Node): IO[Set[Node]]

  def show(showNode: Node => String): IO[String]

}