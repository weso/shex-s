package es.weso.depgraphs

import munit._
import cats._
import cats.implicits._

class DepGraphTest extends FunSuite {

  test("should be able to create empty graph") {
    val emptyGraph = DepGraph.empty[String]
    assertEquals(emptyGraph.nodes, Set[String]())
  }

  test("Should add one element") {
    val emptyGraph = DepGraph.empty[String]
    assertEquals(emptyGraph.addNode("a").nodes, Set("a"))
  }

  test("Should add one edge") {
    val g = DepGraph.empty[String].addPosEdge("a", "b").addNegEdge("a", "c")

    assertEquals(g.outEdges("a"), Set[(PosNeg, String)]((Pos, "b"), (Neg, "c")).asRight[String])
  }

  test("Should calculate if graph has neg cycles a-(+)->b, a-(-)->c: false") {
    val g = DepGraph.empty[String].addPosEdge("a", "b").addNegEdge("a", "c")
    assertEquals(g.containsNegCycle, false)
  }

  test("Should calculate if graph has neg cycles when it hasn't") {
    val g = DepGraph
      .empty[String]
      .addPosEdge("a", "b")
      .addNegEdge("a", "c")
      .addPosEdge("b", "d")
      .addPosEdge("d", "a")
    assertEquals(g.containsNegCycle, false)
  }

  test("Should be able to add pos and neg edges between the same nodes") {
    val g = DepGraph.empty[String].addPosEdge("a", "a").addNegEdge("a", "a")
    assertEquals(g.containsNegCycle, true)
  }

  test("Should calculate neg cycles") {
    val g = DepGraph
      .empty[String]
      .addNegEdge("a", "b")
      .addPosEdge("a", "c")
      .addPosEdge("b", "d")
      .addPosEdge("d", "a")
    val cycles = g.negCycles
    assertEquals(cycles.size, 1)
    val cycle = cycles.head
    assertEquals(cycle, Set(("a", "b"), ("b", "d"), ("d", "a")))
    assertEquals(g.containsNegCycle, true)
  }

  test("Should count negCycles with 2") {
    val g = DepGraph.empty[String].addNegEdge("s", "t").addNegEdge("t", "u").addPosEdge("u", "s")
    assertEquals(g.containsNegCycle, true)
    assertEquals(g.countNegLinks(Set(("s", "t"), ("t", "u"), ("u", "s"))), 2)
  }

  test("Should count negCycles with 3") {
    val g = DepGraph.empty[String].addNegEdge("s", "t").addNegEdge("t", "u").addNegEdge("u", "s")
    assertEquals(g.containsNegCycle, true)
    assertEquals(g.countNegLinks(Set(("s", "t"), ("t", "u"), ("u", "s"))), 3)
  }

  {
    val g = DepGraph
      .empty[String]
      .addPosEdge("a", "b")
      .addPosEdge("a", "c")
      .addPosEdge("b", "d")
      .addPosEdge("d", "c")
      .addPosEdge("c", "d")
    test(s"Should calculate inEdges of a node with no ones") {
      g.inEdges("a")
        .fold(
          s => fail(s"Error obtaining inEdges of a: $s"),
          ins => assertEquals(ins, Set[(String, PosNeg)]())
        )
    }

    test(s"Should fail with non existing node") {
      g.inEdges("x")
        .fold(
          s => (),
          ins => fail(s"Finds inEdges of non existing node. Finds: $ins")
        )
    }

    test(s"Should find one ") {
      g.inEdges("b")
        .fold(
          s => fail(s"Fails with node b: $s"),
          ins => assertEquals(ins, Set[(String, PosNeg)](("a", Pos)))
        )
    }

    test(s"Should find two for d with cycles") {
      g.inEdges("d")
        .fold(
          s => fail(s"Fails with node d: $s"),
          ins => assertEquals(ins, Set[(String, PosNeg)](("c", Pos), ("b", Pos)))
        )
    }

  }

}
