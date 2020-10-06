package es.weso.depgraphs

import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class DepGraphTest
  extends AnyFunSpec
  with Matchers
  with EitherValues {

  describe("A Graph") {

    it("should be able to create empty graph") {
      val emptyGraph = DepGraph.empty[String]
      emptyGraph.nodes should contain theSameElementsAs Set()
    }

    it("Should add one element") {
      val emptyGraph = DepGraph.empty[String]
      emptyGraph.addNode("a").nodes should contain theSameElementsAs Set("a")
    }

    it("Should add one edge") {
      val g = DepGraph.empty[String].
        addPosEdge("a", "b").
        addNegEdge("a", "c")

      g.outEdges("a").right.value should contain theSameElementsAs
        Set((Pos, "b"), (Neg, "c"))
    }

    it("Should calculate if graph has neg cycles a-(+)->b, a-(-)->c: false") {
      val g = DepGraph.empty[String].
        addPosEdge("a", "b").
        addNegEdge("a", "c")
      g.containsNegCycle should be(false)
    }

    it("Should calculate if graph has neg cycles when it hasn't") {
      val g = DepGraph.empty[String].
        addPosEdge("a", "b").
        addNegEdge("a", "c").
        addPosEdge("b", "d").
        addPosEdge("d", "a")
      g.containsNegCycle should be(false)
    }

    it("Should be able to add pos and neg edges between the same nodes") {
      val g = DepGraph.empty[String].
        addPosEdge("a", "a").
        addNegEdge("a", "a")
      g.containsNegCycle should be(true)
    }


    it("Should calculate neg cycles") {
      val g = DepGraph.empty[String].
        addNegEdge("a", "b").
        addPosEdge("a", "c").
        addPosEdge("b", "d").
        addPosEdge("d", "a")
      val cycles = g.negCycles
      cycles should have size 1
      val cycle = cycles.head
      cycle should contain theSameElementsAs Set(("a","b"), ("b","d"), ("d","a"))
      g.containsNegCycle should be(true)
    }

    it("Should count negCycles with 2") {
      val g = DepGraph.empty[String].
        addNegEdge("s", "t").
        addNegEdge("t", "u").
        addPosEdge("u","s")
        g.containsNegCycle should be(true)
        g.countNegLinks(Set(("s","t"),("t","u"),("u","s"))) should be(2)
    }

    it("Should count negCycles with 3") {
      val g = DepGraph.empty[String].
        addNegEdge("s", "t").
        addNegEdge("t", "u").
        addNegEdge("u","s")
      g.containsNegCycle should be(true)
      g.countNegLinks(Set(("s","t"),("t","u"),("u","s"))) should be(3)
    }
  }

  describe(s"inEdges") {
    val g = DepGraph.empty[String].
      addPosEdge("a","b").
      addPosEdge("a","c").
      addPosEdge("b","d").
      addPosEdge("d","c").
      addPosEdge("c","d")
    it(s"Should calculate inEdges of a node with no ones") {
      g.inEdges("a").fold(
        s => fail(s"Error obtaining inEdges of a: $s"),
        ins => ins should contain theSameElementsAs Set[(String, String)]()
      )
    }

    it(s"Should fail with non existing node") {
        g.inEdges("x").fold(
          s => info(s"Fails with non existing node: x, $s"),
          ins => fail(s"Finds inEdges of non existing node. Finds: $ins")
        )
    }

    it(s"Should find one ") {
        g.inEdges("b").fold(
          s => fail(s"Fails with node b: $s"),
          ins => ins should contain theSameElementsAs Set(("a", Pos))
        )
    }
    it(s"Should find two for d with cycles") {
      g.inEdges("d").fold(
        s => fail(s"Fails with node d: $s"),
        ins => ins should contain theSameElementsAs Set(("c",Pos),("b", Pos))
      )
    }

  }

}

