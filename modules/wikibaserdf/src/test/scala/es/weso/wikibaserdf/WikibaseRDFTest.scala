package es.weso.wikibaserdf

import WikibaseRDF._
import cats.effect.IO
import es.weso.rdf.triples.RDFTriple
import es.weso.shex.Schema
import es.weso.shex.ResolvedSchema
import es.weso.shapemaps.QueryShapeMap
import es.weso.shapemaps.ShapeMap
import cats.data._
import cats.implicits._
import es.weso.utils.IOUtils._
import es.weso.shex.validator.Validator
import es.weso.shapemaps.ResultShapeMap
import es.weso.rdf.nodes.IRI
import es.weso.rdf.PREFIXES._
import es.weso.shapemaps.IRILabel
import es.weso.rdf.jena.RDFAsJenaModel
import munit._

class WikibaseRDFTest extends CatsEffectSuite {

  val slow = new munit.Tag("Slow")

  test(s"Should obtain triples for an item".tag(slow)) {
    val r: IO[List[RDFTriple]] = WikibaseRDF.wikidata.flatMap(
      _.use(wikibase =>
        for {
          ts <- wikibase.triplesWithSubject(wd + "Q42").compile.toList
        } yield ts
      )
    )
    r.map(vs => assertEquals(vs.length > 0, true))
  }

  {
    val item = IRI("http://www.wikidata.org/entity/Q29377880")

    test(s"Should obtain triples for an item".tag(slow)) {
      val r: IO[(List[RDFTriple], List[RDFTriple], List[RDFTriple], CachedState)] =
        WikibaseRDF.wikidata.flatMap(
          _.use(wikibase =>
            for {
              ts1 <- wikibase.triplesWithSubject(wd + "Q42").compile.toList
              ts2 <- wikibase.triplesWithSubject(wd + "Q42").compile.toList
              ts3 <- wikibase.triplesWithSubject(wd + "Q42").compile.toList
              cs <- wikibase.refCached.get
            } yield (ts1, ts2, ts3, cs)
          )
        )
      r.map { tuple =>
        val (ts1, ts2, ts3, cs) = tuple
        println(s"Triples: ${ts1.length}, ${ts2.length}\nCachedState: ${cs.iris.mkString(",")}")
      }
    }

    /*    it(s"Should use wikibase rdf once with the same item cached") {
       val r: IO[(Int,Int)] = WikibaseRDF.wikidata.flatMap(_.use(wd => for {
           ts1 <- wd.triplesWithSubject(item).compile.toList
           ts2 <- wd.triplesWithSubject(item).compile.toList
       } yield ((ts1.length, ts2.length))))
       r.attempt.unsafeRunSync.fold(
         s => s"Error: ${s.getMessage}",
         pair => {
          val (n1,n2) = pair
          n1 should be (n2)
         }
       )
   }

   it(s"Should use wikibase rdf twice") {
       val r: IO[((Int,Int),(Int,Int))] = for {
         res <- WikibaseRDF.wikidata
         pair1 <- res.use(wd => for {
           _ <- wd.showRDFId("1st use")
           ts1 <- wd.triplesWithSubject(item).compile.toList
           ts2 <- wd.triplesWithSubject(item).compile.toList
         } yield ((ts1.length, ts2.length)))
         _ <- IO(pprint.pprintln("======================="))
         res2 <- WikibaseRDF.wikidata
         pair2 <- res2.use(wd => for {
           _ <- wd.showRDFId("2nd use")
           ts1 <- wd.triplesWithSubject(item).compile.toList
           ts2 <- wd.triplesWithSubject(item).compile.toList
         } yield ((ts1.length, ts2.length)))
       } yield (pair1,pair2)
       r.attempt.unsafeRunSync.fold(
         s => fail(s"Error: ${s.getMessage}"),
         ppair => {
          val (n1,n2) = ppair
          info(s"Pairs: $ppair")
          n1 should be (n2)
         }
       )
   }

  ignore(s"Should obtain 2 empty RDFAsJenaModel's...")  {

     val r: IO[Unit] = RDFAsJenaModel.empty.flatMap(_.use { case rdf1 => for {
         model1 <- rdf1.getModel
         _ <- IO (pprint.log(s"RDF1: ${rdf1}. IsClosed?: ${model1.isClosed}"))
         _ <- RDFAsJenaModel.empty.flatMap(_.use { rdf2 => for {
          model2 <- rdf2.getModel
          _ <- IO(pprint.log(s"RDF1 (inside for): ${rdf1}. IsClosed?: ${model1.isClosed}"))
          _ <- IO(pprint.log(s"RDF2 (inside for): ${rdf2}. IsClosed?: ${model2.isClosed}"))
         } yield () })
      } yield () })
      r.attempt.unsafeRunSync.fold(
          s => fail(s"Error: $s"),
          _ => info(s"Finnished")
      )
  }

  }

  describe(s"Validate Wikidata items") {
   {
    val ex = IRI("http://example.org/")
    val strSchema =
      s"""|prefix : <${ex.str}>
          |prefix wdt: <${wdt.str}>
          |prefix xsd: <${xsd.str}>
          |
          |:S {
          |  wdt:P31 IRI +
          |}""".stripMargin
    println(s"strSchema: \n${strSchema}")
    shouldValidateWikidata(wd+"Q42", ex+"S", strSchema, true, Some(ex))
   }
   {
    val ex = IRI("http://example.org/")
    val strSchema =
      s"""|PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
          |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
          |PREFIX wd: <http://www.wikidata.org/entity/>
          |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
          |
          |start = @<human>
          |
          |<human> EXTRA wdt:P31 {
          |  wdt:P31 [wd:Q5];
          |  wdt:P21 [wd:Q6581097 wd:Q6581072 wd:Q1097630 wd:Q1052281 wd:Q2449503 wd:Q48270]?;   # gender
          |  wdt:P19 . ?;                     # place of birth
          |  wdt:P569 . ? ;                 # date of birth
          |  wdt:P735 . * ;                 # given name
          |  wdt:P734 . * ;                 # family name
          |  wdt:P106 . * ;                 # occupation
          |  wdt:P27 @<country> *;  # country of citizenship
          |  wdt:P22 @<human> *;           # father
          |  wdt:P25 @<human> *;           # mother
          |  wdt:P3373 @<human> *;         # sibling
          |  wdt:P26 @<human> *;           # husband/wife
          |  wdt:P40 @<human> *;           # children
          |  wdt:P1038 @<human> *;         # relatives
          |  wdt:P103 @<language> *;
          |  wdt:P1412 @<language> *;
          |  wdt:P6886  @<language> *;
          |  rdfs:label rdf:langString +;
          |}
          |
          |<country> EXTRA wdt:P31 {
          |  wdt:P31 [ wd:Q6256 wd:Q3024240 wd:Q3624078] +;
          |}
          |
          |#<language> EXTRA wdt:P31 {
          |#  wdt:P31 [wd:Q34770 wd:Q1288568] +;
          |#}
          |<language> { }
          |""".stripMargin
    println(s"strSchema: \n${strSchema}")
    shouldValidateWikidata(wd+"Q42", IRI("human"), strSchema, true, Some(ex))
  }
  }

 private def fromEitherES[A](e: Either[String,A]): IO[A] =
   IO.fromEither(e.leftMap(s => new RuntimeException(s"Error: $s")))

 def shouldValidateWikidata(entity: IRI, label: IRI, schemaStr: String, expected: Boolean, base: Option[IRI]): Unit = {

  // TODO: We ignore this test because it takes a lot of time
   ignore(s"Should validate ${entity} with ${schemaStr} and obtain ${expected}") {
     println(s"Inside should...")
     val r: IO[ResultShapeMap] = for {
       res1 <- WikibaseRDF.wikidata
       res2 <- RDFAsJenaModel.empty
       vv <- (res1, res2).tupled.use{ case (wikibase,builder) =>
        for {
         schema <- Schema.fromString(schemaStr, "ShExC", base)
         resolvedSchema <- ResolvedSchema.resolve(schema, base)
         shapeMapStr = s"<${entity.str}>@<${label.str}>"
         shapeMap <- fromEitherES(ShapeMap.fromString(shapeMapStr,
            "Compact",
            base,
            wikibase.prefixMap,
            resolvedSchema.prefixMap))
         _ <- IO { println(s"ShapeMap obtained ${shapeMap}"); IO.pure(()) }
         fixedShapeMap <- ShapeMap.fixShapeMap(shapeMap,wikibase,wikibase.prefixMap,resolvedSchema.prefixMap)
         result <- Validator.validate(resolvedSchema,fixedShapeMap,wikibase,builder)
         resultShapeMap <- result.toResultShapeMap
        } yield (resultShapeMap) }
      } yield vv
      r.attempt.unsafeRunSync.fold(
        s => fail(s"Error running validation: ${s}"),
        result => {
          val iriLabel = IRILabel(base.fold(label)(_.resolve(label)))
          println(s"Result: ${result}\nExpected label:${iriLabel}\nEntity: ${entity}\nConformant shapes: ${result.getConformantShapes(entity)}")
          println(s"Condition: ${result.getConformantShapes(entity) contains iriLabel}")
          (result.getConformantShapes(entity) contains iriLabel, expected) match {
            case (false,false) => info(s"Failed to validate ${entity} as ${label} as expected")
            case (true,true) => info(s"Validated ${entity} as ${label} as expected")
            case (false,true) => fail(s"Failed to validate ${entity} as ${label}\nResult: ${result}\nResult in JSON:\n${result.toJson.spaces2}")
            case (true,false) => fail(s"${entity} conforms to ${label} but it was expected to fail\nResult: ${result}")
          }
          }
      )
   }
     */
  }

}
