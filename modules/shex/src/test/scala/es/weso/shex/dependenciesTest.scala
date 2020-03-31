package es.weso.shex

import es.weso.depgraphs.{DepGraph, Neg, Pos, PosNeg}
import es.weso.rdf.nodes._
<<<<<<< HEAD
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
=======
import org.scalatest._
//import es.weso.utils.eitherios.EitherIOUtils._
import cats.data.EitherT
// import cats._
// import cats.implicits._
import cats.effect.IO
import matchers.should._
import funspec._
>>>>>>> issue57

class dependenciesTest extends AnyFunSpec with Matchers with EitherValues {

  // Some common values
  val e = "http://example.org/"
  val r: ShapeLabel = IRILabel(IRI(e + "R"))
  val s: ShapeLabel = IRILabel(IRI(e + "S"))
  val t: ShapeLabel = IRILabel(IRI(e + "T"))
  val u: ShapeLabel = IRILabel(IRI(e + "U"))

  ignore("Dependency graph") {

    def depGraphTest(schema: String, expectedGraph: Set[(ShapeLabel, Set[(PosNeg, ShapeLabel)])]): Unit = {
      it(s"should check that dependency graph of $schema matches $expectedGraph") {
        val expectedDepGraph = DepGraph.makeGraph(expectedGraph)
        val v: EitherT[IO, String, (Schema, DepGraph[ShapeLabel])] = for {
          schema <- EitherT.liftF(Schema.fromString(schema, "SHEXC",None))
          depGraph <- EitherT.fromEither[IO](schema.depGraph) 
          iso <- EitherT.fromEither[IO](depGraph.isomorphicWith(expectedDepGraph))
        } yield (schema,depGraph)
        v.value.unsafeRunSync.fold(s => fail(s"Error: $s"), ps => {
          info(s"Dependency passes: $ps")
        })
      }
    }

    depGraphTest(
      s"""prefix : <$e>
       |:S { :p @:T }
     """.stripMargin, Set((s, Set((Pos, t)))))

    depGraphTest(
      s"""prefix : <$e>
      |:S { :p @:S }
     """.stripMargin, Set((s, Set((Pos, s)))))

    depGraphTest(
      s"""prefix : <$e>
         |:S NOT { :p @:S }
     """.stripMargin, Set((s, Set((Neg, s)))))

    depGraphTest(
      s"""prefix : <$e>
         |:S { :p @:T; :q NOT @:R }
     """.stripMargin,
      Set((s, Set((Pos, t), (Neg, r)))))


    depGraphTest(
      s"""prefix : <$e>
         |:S { :p NOT @:T }
         |:T NOT @:U
         |:U { :b @:S }
     """.stripMargin,
      Set(
        (s, Set((Neg, t))),
        (t, Set((Neg, u))),
        (u, Set((Pos, s)))
      ))


    depGraphTest(
      s"""prefix : <$e>
         |:S EXTRA :p { :p @:T }
     """.stripMargin,
      Set((s, Set((Pos, t), (Neg, t)))))

  }

  describe("Negative cycles test") {

    def negCyclesTest(schemaStr: String, negCyclesLabels: Set[Set[(ShapeLabel,ShapeLabel)]]): Unit = {
      it(s"should check that negCycles of schema: \n$schemaStr are:\n$negCyclesLabels") {
        val r : EitherT[IO,String, (Set[Set[(ShapeLabel,ShapeLabel)]], String)] = for {
          schema <- EitherT.liftF(Schema.fromString(schemaStr, "SHEXC", None)).leftMap( (s:String) => s"Error $s parsing $schemaStr")
          cycles <- EitherT.fromEither[IO](schema.oddNegCycles).leftMap(s => s"Error $s calculating negCycles of $schemaStr")
          graphStr = schema.depGraph.map(_.showEdges()).getOrElse("<empty>")
        } yield (cycles, graphStr)

        r.value.unsafeRunSync.fold(msg => fail(msg), pair => {
          val (cycles, graphStr) = pair
          (negCyclesLabels.isEmpty, cycles.isEmpty) match {
            case (false,false) => info("No odd neg cycles as expected")
            case (true,false) => fail(s"Dependency graph = ${graphStr}\nExpected no negCycles but found neg cycles: \n$cycles")
            case (false,true) => fail(s"Dependency graph = ${graphStr}\nExpected negCycles to be \n${negCyclesLabels} but found neg cycles")
            case (true,true) => cycles should contain theSameElementsAs (negCyclesLabels)
          }
        })
      }
    }

      negCyclesTest(
      """
      |prefix : <http://example.org/>
      |
      |:S { :p @:T }
      |:T { :p @:S }
    """.
        stripMargin, Set())

    negCyclesTest(
      """prefix : <http://example.org/>
      |:S { :p @:T } AND NOT @:R
      |:T { :p @:S }
    """.
        stripMargin, Set())

/*    negCyclesTest(
      """prefix : <http://example.org/>
        |:S { :p @:T } AND NOT @:R
        |:R { :q @:S }
        |:T { :p @:S }
      """.
        stripMargin, Set(Set((s, r)))) */

    negCyclesTest(
      s"""prefix : <$e>
         |:S { :p NOT @:T }
         |:T NOT @:U
         |:U { :b @:S }
     """.stripMargin,
      Set())

    negCyclesTest(
      s"""|PREFIX :       <http://example.org/>
          |:S EXTRA :a { :a @:T }
          |:T { :b . }
     """.stripMargin,
      Set())

    negCyclesTest(
      s"""|PREFIX :       <http://example.org/>
          |:S EXTRA :a { :a @:S }
     """.stripMargin,
      Set(Set((s,s))))

    negCyclesTest(
      s"""|PREFIX :       <http://example.org/>
          |:S { :a NOT @:T }
          |:T { :a NOT @:U }
          |:U { :a NOT @:S }
     """.stripMargin,
      Set(Set((s, t), (u,s), (t,u)))
    )

    negCyclesTest(
      s"""|PREFIX :       <http://example.org/>
          |:S { :a NOT @:T }
          |:T { :a NOT @:U }
          |:U { :a @:S }
     """.stripMargin,
      Set())

  }

}
