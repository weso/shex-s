package es.weso.depgraphs

import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class InheritanceTest
  extends AnyFunSpec
  with Matchers
  with EitherValues {

  describe("An inheritance Graph") {

    it("should be able to create empty graph") {
      val cmp = InheritanceJGraphT.empty[String].flatMap(_.nodes)
      cmp.unsafeRunSync should contain theSameElementsAs Set()
    }

    it("Should add one element") {
      val cmp = for {
        g <- InheritanceJGraphT.empty[String]
        _ <- g.addNode("a")
        ns <- g.nodes
      } yield ns
      cmp.unsafeRunSync should contain theSameElementsAs Set("a")
    }

    it("Should get descendants") {
      val cmp = for {
        g <- InheritanceJGraphT.empty[String]
        _ <- g.addInheritance("A","B")
        _ <- g.addInheritance("B","C")
        ds <- g.descendants("A")
      } yield ds
      cmp.unsafeRunSync() should contain theSameElementsAs List("B","C")
    }

    it("Should get ancestors") {
      val cmp = for {
        g <- InheritanceJGraphT.empty[String]
        _ <- g.addInheritance("A","B")
        _ <- g.addInheritance("B","C")
        asA <- g.ancestors("A")
        asB <- g.ancestors("B")
        asC <- g.ancestors("C")
      } yield (asA,asB,asC)
      cmp.attempt.unsafeRunSync().fold(
        e => fail(s"Error: ${e.getMessage}"),
        ps => {
          val (asA,asB,asC) = ps
          asA should contain theSameElementsAs List()
          asB should contain theSameElementsAs List("A")
          asC should contain theSameElementsAs List("A","B")
        }
      ) 
    }

    it("Should get ancestors ordered?") {
      val cmp = for {
        g <- InheritanceJGraphT.empty[String]
        _ <- g.addInheritance("A","B")
        _ <- g.addInheritance("A","C")
        _ <- g.addInheritance("C","D")
        _ <- g.addInheritance("D","B")
        asA <- g.ancestors("A")
        asB <- g.ancestors("B")
        asC <- g.ancestors("C")
        asD <- g.ancestors("D")
      } yield (asA,asB,asC,asD)
      cmp.attempt.unsafeRunSync().fold(
        e => fail(s"Error: ${e.getMessage}"),
        ps => {
          val (asA,asB,asC,asD) = ps
          asA should contain theSameElementsAs List()
          asB should contain theSameElementsAs List("A","C","D")
          asC should contain theSameElementsAs List("A")
          asD should contain theSameElementsAs List("A","C")
        }
      ) 
    }

    it("Should fail with cycles") {
      val cmp = for {
        g <- InheritanceJGraphT.empty[String]
        _ <- g.addInheritance("A","B")
        _ <- g.addInheritance("B","A")
        asA <- g.ancestors("A")
        asB <- g.ancestors("B")
      } yield (asA,asB)
      cmp.attempt.unsafeRunSync().fold(
        e => info(s"Cycle detected: ${e.getMessage}"),
        ps => fail(s"Should fail but obtained ${ps}")
      ) 
    }    

    it("Should fail without nodes") {
      val cmp = for {
        g <- InheritanceJGraphT.empty[String]
        asA <- g.ancestors("A")
      } yield (asA)
      cmp.attempt.unsafeRunSync().fold(
        e => fail(s"Failed with error ${e}"),
        ps => ps should contain theSameElementsAs List()
      ) 
    }    


  }


}

