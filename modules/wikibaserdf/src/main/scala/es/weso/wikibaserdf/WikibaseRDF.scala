package es.weso.wikibaserdf
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.utils.IOUtils._
import es.weso.utils.StreamUtils._
import es.weso.rdf.triples.RDFTriple
import cats.effect._
import fs2.Stream
import es.weso.utils.Deref._
import es.weso.rdf.path.SHACLPath
import org.apache.jena.rdf.model.{RDFNode => JenaRDFNode}
import cats.effect.concurrent.Ref
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.rdf.jena.SPARQLQueries._
import org.apache.jena.query.QueryExecutionFactory
import org.apache.jena.query._
import es.weso.rdf.jena.JenaMapper._
import es.weso.utils.internal.CollectionCompat.CollectionConverters._
import cats.implicits._
import scala.util._
import java.io.ByteArrayOutputStream
import io.circe.Json
import io.circe.parser.parse

case class CachedState(iris: Set[IRI], rdf: RDFAsJenaModel) 

object CachedState {
  def initial: IO[CachedState] = RDFAsJenaModel.empty.use(rdf => IO(CachedState(Set(),rdf)))
}

case class WikibaseRDF(
    endpoint: IRI,
    prefixMap: PrefixMap, 
    refCached: Ref[IO,CachedState] 
) extends RDFReader {

 // val reader: RDFAsJenaModel = RDFAsJenaModel(cachedModel,None,None)

 override def getPrefixMap: IO[PrefixMap] = IO(prefixMap)

 /*override def fromString(cs: CharSequence, format: String, base: Option[IRI]): RDFRead[Rdf] = {
    err("Cannot parse WikibaseRDF")
  }*/

 private def getCachedState: IO[CachedState] = refCached.get

 override def serialize(format: String,
                         base: Option[IRI]): RDFRead[String] = for {
    cached <- getCachedState                             
 } yield {
   s"Wikidata endpoint\nCached nodes:\n${cached.iris.mkString("\n")}"
 }
 
 override def rdfTriples(): RDFStream[RDFTriple] = {
    errStream("Cannot obtain triples from WikibaseRDF ")
 }

 private def addNodeCache(iri: IRI, newRdf: RDFAsJenaModel): IO[Unit] = for {
   cachedState <- refCached.get
   merged  <- cachedState.rdf.merge(newRdf)
   _ <- refCached.modify(cachedState => { 
      val newIris = cachedState.iris + iri
      (CachedState(newIris,merged),())
    })
 } yield (())

    

 override def triplesWithSubject(node: RDFNode): RDFStream[RDFTriple] = for {
   cachedState <- Stream.eval(refCached.get)
   ts <- node match {
     case subj: IRI => for {
         cachedState <- Stream.eval(refCached.get)
         r <- if (cachedState.iris contains subj) {
           println(s"Node ${node} already in cache")
           cachedState.rdf.triplesWithSubject(node) 
         } else for {
//           _ <- Stream.eval { IO { println(s"Dereferentiating ${node}") ; IO.pure(())} }
           rdfNode <- Stream.resource(derefRDFJava(subj))
           _ <- Stream.eval(addNodeCache(subj, rdfNode))
           rs <- rdfNode.triplesWithSubject(subj)
           } yield rs
          } yield r   
     case other => cachedState.rdf.triplesWithSubject(other)
   }
 } yield ts
 

 override def asRDFBuilder: IO[RDFBuilder] = err(s"No RDFBuilder for WikibaseRDF")
 override def availableParseFormats: List[String] = List()
 override def availableSerializeFormats: List[String] = List()

 override def checkDatatype(node: RDFNode,datatype: IRI): IO[Boolean] = for {
   cachedState <- refCached.get
   b <- cachedState.rdf.checkDatatype(node,datatype)
 } yield b

 override def triplesWithObject(node: RDFNode): RDFStream[RDFTriple] = node match {
    case obj: IRI => {
      val model = QueryExecutionFactory.sparqlService(endpoint.str, queryTriplesWithObject(obj)).execConstruct()
      streamFromIOs(model2triples(model))
    }
    case _ => errStream("triplesWithObject: node " + node + " must be a IRI")
  }
  
 override def triplesWithPredicate(p: IRI): Stream[IO,RDFTriple] = {
    val model = QueryExecutionFactory.sparqlService(endpoint.str, queryTriplesWithPredicate(p)).execConstruct()
    streamFromIOs(model2triples(model))
  }

  override def triplesWithPredicateObject(p: IRI,o: RDFNode): Stream[IO,RDFTriple] = o match {
    case iri: IRI => {
      val model = QueryExecutionFactory.sparqlService(endpoint.str, queryTriplesWithPredicateObject(p, iri)).execConstruct()
      streamFromIOs(model2triples(model))
    }
    case _ => errStream("triplesWithPredicateObject: o " + o + " must be a IRI")
  }

 override def hasPredicateWithSubject(n: RDFNode,p: IRI): IO[Boolean] = err(s"Endpoint: Not implemented hasPredicateWithSubject")

 override def getNumberOfStatements(): IO[Int] = err(s"Not implemented getNumberOfStatements for WikibaseRDF")
 
 override def getSHACLInstances(cls: RDFNode): Stream[IO,RDFNode] = {
    cls match {
      case iri: IRI => try {
        val resultSet = QueryExecutionFactory.sparqlService(endpoint.str, queryShaclInstances(iri)).execSelect()
        val rs = resultSet.asScala.map(qs => qs.get("x") match {
          case null => s"Not found value for variable in querySolution: $qs".asLeft[RDFNode]
          case r => {
            val node: RDFNode = IRI(r.asResource.getURI)
            node.asRight
          }
        }).toList
        rs.sequence.fold(errStream(_), Stream.emits(_))
      } catch {
        case e: Exception => errStream(s"getSHACLInstances: ${e.getMessage}")
      }
      case l: Literal => Stream.empty
      case bn => errStream(s"getSHACLInstances not implemented for blank node $cls on endpoint ${endpoint}")
    }
  }

 override def hasSHACLClass(n: RDFNode,c: RDFNode): IO[Boolean] = (n,c) match {
    case (iriN: IRI, iriC: IRI) => {
      val b = QueryExecutionFactory.sparqlService(endpoint.str, queryHasShaclClass(iriN,iriC)).execAsk()
      ok(b)
   }
    case _ => ok(false)
  }

 override val id: String = s"WikibaseRDF(${endpoint.str})"
 
 override def isIsomorphicWith(other: RDFReader): cats.effect.IO[Boolean] = IO.pure(false)

 override def nodesWithPath(p: SHACLPath): Stream[IO,(RDFNode, RDFNode)] = errStream(s"Not implemented nodesWithPath for wikibaseRDF")

 override def objectsWithPath(subj: es.weso.rdf.nodes.RDFNode,path: es.weso.rdf.path.SHACLPath): fs2.Stream[cats.effect.IO,es.weso.rdf.nodes.RDFNode] = ???

 override def queryAsJson(str: String): cats.effect.IO[io.circe.Json] = Try {
    val query = QueryFactory.create(str)
    val qExec = QueryExecutionFactory.sparqlService(endpoint.str, query)
    qExec.getQuery.getQueryType match {
      case Query.QueryTypeSelect => {
        val result = qExec.execSelect()

        // val prologue = qExec.getQuery.getPrologue
        // val prefixMap: Map[String,String] = prologue.getPrefixMapping.getNsPrefixMap.asScala.toMap

        // TODO: Add prefixes and base to JSON result
//        val prefixes = PrefixMap(prefixMap.map { case (k,v) => (Prefix(k), IRI(v)) })
//        val base = prologue.getBaseURI()
        val outputStream = new ByteArrayOutputStream()
        ResultSetFormatter.outputAsJSON(outputStream, result)
        val jsonStr = new String(outputStream.toByteArray())
        /* val result = parse(jsonStr).leftMap(f => f.getMessage)
        val json = Json.fromFields(
          List(
            ("base", prologue.getBaseURI),
            ("prefixes", jsonPrefixes), 
            ("result", result) 
        ))
        json */
        parse(jsonStr).leftMap(f => f.getMessage)
      }
      case Query.QueryTypeConstruct => {
        // val result = qExec.execConstruct()
        Left(s"Unimplemented CONSTRUCT queries yet")
      }
      case Query.QueryTypeAsk => {
        val result = qExec.execAsk()
        Right(Json.fromBoolean(result))
      }
      case Query.QueryTypeDescribe => {
        Left(s"Unimplemented DESCRIBE queries yet")
      }
      case _ => {
        Left(s"Unknown type of query. Not implemented")
      }
    }
  }.fold(f => err(f.getMessage), _.fold(err(_), ok(_)))


 override def querySelect(queryStr: String): RDFStream[Map[String,RDFNode]] = {
    Try {
      val query = QueryFactory.create(queryStr)
      val qExec = QueryExecutionFactory.sparqlService(endpoint.str, query)
      qExec.getQuery.getQueryType match {
        case Query.QueryTypeSelect => {
          val result = qExec.execSelect()
          val ls: List[IO[Map[String, RDFNode]]] = 
           result.asScala.toList.map(qs => {
            val qsm = new QuerySolutionMap()
            qsm.addAll(qs)
            val pairs: List[(String, JenaRDFNode)] =
             qsm.asMap.asScala.view.toMap.toList
            val iom: IO[Map[String, RDFNode]] = 
             pairs.map { 
               case (v, jenaNode) => jenaNode2RDFNode(jenaNode).flatMap(node => ok((v, node))) 
              }.sequence.map(_.toMap)
            iom 
          })
          ls.sequence
        }
        case qtype => throw new Exception(s"Query ${queryStr} has type ${qtype} and must be SELECT query ")
      }
    }.fold(Stream.raiseError[IO], fromIOLs)
  }

 
 override def rdfReaderName: String = s"WikibaseRDF"

 override def sourceIRI: Option[IRI] = None

 override def subjectsWithPath(p: SHACLPath,o: RDFNode): Stream[IO,RDFNode] = errStream(s"Not implemented subjectsWithPath for WikibaseRDF")


}

object WikibaseRDF {
  val wd = IRI("http://www.wikidata.org/entity/")
  val wdt = IRI("http://www.wikidata.org/prop/direct/")
  val wikidataEndpoint = IRI("https://query.wikidata.org/sparql")
  val wikidata : IO[WikibaseRDF] = for {
    initial <- CachedState.initial
    ref <- Ref[IO].of(initial)
  } yield WikibaseRDF(wikidataEndpoint,prefixMap = PrefixMap(Map(
    Prefix("wd") -> wd,
    Prefix("wdt") -> wdt
   )), ref)

}