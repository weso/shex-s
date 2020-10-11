package es.weso.wikibaserdf

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import WikibaseRDF._
import cats.effect.IO
import es.weso.rdf.triples.RDFTriple
import es.weso.shex.Schema
import es.weso.shex.ResolvedSchema
import es.weso.shapeMaps.QueryShapeMap
import es.weso.shapeMaps.ShapeMap
import cats.data._  
import es.weso.utils.IOUtils._
import es.weso.shex.validator.Validator
import es.weso.shapeMaps.ResultShapeMap
import es.weso.rdf.nodes.IRI
import es.weso.rdf.PREFIXES._
import es.weso.shapeMaps.IRILabel

class WikibaseRDFTest extends AnyFunSpec with Matchers {

  describe(s"Test Wikidata subjects") {
    it(s"Should obtain triples for an item") {
      val r: IO[List[RDFTriple]] = for {
        wikibase <- WikibaseRDF.wikidata
        ts <- wikibase.triplesWithSubject(wd + "Q42").compile.toList
      } yield ts
      r.attempt.unsafeRunSync.fold(
        s => s"Error: ${s.getMessage}",
        vs => info(s"Triples: ${vs.length}")
      )
    }
  }

  describe(s"Test Wikidata subjects twice") {
    it(s"Should obtain triples for an item") {
      val r: IO[(List[RDFTriple],List[RDFTriple],List[RDFTriple],CachedState)] = for {
        wikibase <- WikibaseRDF.wikidata
        ts1 <- wikibase.triplesWithSubject(wd + "Q42").compile.toList
        ts2 <- wikibase.triplesWithSubject(wd + "Q42").compile.toList
        ts3 <- wikibase.triplesWithSubject(wd + "Q42").compile.toList
        cs <- wikibase.refCached.get
      } yield (ts1,ts2,ts3,cs)
      r.attempt.unsafeRunSync.fold(
        s => s"Error: ${s.getMessage}",
        tuple => { 
          val (ts1,ts2,ts3,cs) = tuple
          info(s"Triples: ${ts1.length}, ${ts2.length}\nCachedState: ${cs.iris.mkString(",")}") 
      }
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

 def shouldValidateWikidata(entity: IRI, label: IRI, schemaStr: String, expected: Boolean, base: Option[IRI]): Unit = {

  // TODO: We ignore this test because it takes a lot of time
   ignore(s"Should validate ${entity} with ${schemaStr} and obtain ${expected}") {
     println(s"Inside should...")
     val r: EitherT[IO,String,ResultShapeMap] = for {
        wikibase <- io2es(WikibaseRDF.wikidata)
        schema <- io2es(Schema.fromString(schemaStr, "ShExC", base))
        resolvedSchema <- io2es(ResolvedSchema.resolve(schema, base))
        shapeMapStr = s"<${entity.str}>@<${label.str}>"
        shapeMap <- either2es(
          ShapeMap.fromString(shapeMapStr,
            "Compact",
            base,
            wikibase.prefixMap,
            resolvedSchema.prefixMap)
        ).leftMap(s => s"Error parsing shapeMap: $s\nShapeMap: $shapeMapStr")
        _ <- io2es(IO { println(s"ShapeMap obtained ${shapeMap}"); IO.pure(()) })
        fixedShapeMap <- io2es(ShapeMap.fixShapeMap(shapeMap,wikibase,wikibase.prefixMap,resolvedSchema.prefixMap))
        result <- io2es(Validator.validate(resolvedSchema,fixedShapeMap,wikibase))
        resultShapeMap <- io2es(result.toResultShapeMap)
      } yield (resultShapeMap)
      run_es(r).attempt.unsafeRunSync.fold(
        s => fail(s"Error running validation: ${s}"), 
        _.fold(
          s => fail(s"Error: $s"),
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
      ))
   }
 }


}
