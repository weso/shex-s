package es.weso.depgraphs

import munit._

class InheritanceTest
  extends CatsEffectSuite {

  test("should be able to create empty graph") {
      val cmp = InheritanceJGraphT.empty[String, Unit].flatMap(_.nodes)
      assertIO(cmp, Set[String]())
    }

    test("Should add one element") {
      val cmp = for {
        g <- InheritanceJGraphT.empty[String, Unit]
        _ <- g.addNode("a")
        ns <- g.nodes
      } yield ns
      assertIO(cmp, Set("a"))
    }

    test("Should get descendants") {
      val cmp = for {
        g <- InheritanceJGraphT.empty[String,Unit]
        _ <- g.addInheritance("A","B", ())
        _ <- g.addInheritance("B","C", ())
        ds <- g.descendants("A")
      } yield ds
      assertIO(cmp, Set("B","C"))
    }

    test("Should get ancestors") {
      val cmp = for {
        g <- InheritanceJGraphT.empty[String, Unit]
        _ <- g.addInheritance("A","B", ())
        _ <- g.addInheritance("B","C", ())
        asA <- g.ancestors("A")
        asB <- g.ancestors("B")
        asC <- g.ancestors("C")
      } yield (asA,asB,asC)
      cmp.map(ps => {
          val (asA,asB,asC) = ps
          assertEquals(asA, Set[String]())
          assertEquals(asB, Set("A"))
          assertEquals(asC, Set("A","B"))
        }
      ) 
    }

    test("Should get ancestors ordered?") {
      val cmp = for {
        g <- InheritanceJGraphT.empty[String, Unit]
        _ <- g.addInheritance("A","B", ())
        _ <- g.addInheritance("A","C", ())
        _ <- g.addInheritance("C","D", ())
        _ <- g.addInheritance("D","B", ())
        asA <- g.ancestors("A")
        asB <- g.ancestors("B")
        asC <- g.ancestors("C")
        asD <- g.ancestors("D")
      } yield (asA,asB,asC,asD)
      cmp.map(ps => {
          val (asA,asB,asC,asD) = ps
          assertEquals(asA, Set[String]())
          assertEquals(asB, Set("A","C","D"))
          assertEquals(asC, Set("A"))
          assertEquals(asD, Set("A","C"))
        }
      ) 
    }

    test("Should fail with cycles") {
      val cmp = for {
        g <- InheritanceJGraphT.empty[String, Unit]
        _ <- g.addInheritance("A","B", ())
        _ <- g.addInheritance("B","A", ())
        asA <- g.ancestors("A")
        asB <- g.ancestors("B")
      } yield (asA,asB)
      cmp.attempt.map(e => assertEquals(e.isLeft, true))
    }    

    test("Should fail without nodes") {
      val cmp = for {
        g <- InheritanceJGraphT.empty[String, Unit]
        asA <- g.ancestors("A")
      } yield (asA)
      cmp.map(e => assertEquals(e,Set[String]()))
    }    

}

