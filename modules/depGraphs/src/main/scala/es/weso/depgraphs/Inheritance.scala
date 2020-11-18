package es.weso.depgraphs

trait Inheritance[Node] {

  def empty: Inheritance[Node]  

  def nodes: Set[Node]

  def addNode(node: Node): Inheritance[Node]

  def addInheritance(node1: Node, node2: Node): Inheritance[Node]

  def children(node: Node): List[Node]

}