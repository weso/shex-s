package es.weso.wshex.wshex2es

import cats._
import cats.implicits._
import com.typesafe.scalalogging.LazyLogging
import es.weso._
import es.weso.rbe.interval.{IntLimit, Unbounded}
import es.weso.rdf.nodes._
import es.weso.wbmodel.{Property => _, WBLang => _, _}
import es.weso.rbe.interval.IntOrUnbounded
import scala.collection.compat._ // Required for partitionMap
import es.weso.rdf.nodes._
import es.weso.wshex._
import es.weso.wshex.TermConstraint._

case class WShEx2ESConvertOptions(
    entityIri: IRI,
    directPropertyIri: IRI,
    propIri: IRI,
    propStatementIri: IRI,
    propQualifierIri: IRI,
    entityAlias: String,
    directPropertyAlias: String,
    propAlias: String,
    propStatementAlias: String,
    propQualifierAlias: String
)

object WShEx2ESConvertOptions {
  val default = WShEx2ESConvertOptions(
    entityIri = IRI("http://www.wikidata.org/entity/"),
    directPropertyIri = IRI("http://www.wikidata.org/prop/direct/"),
    propIri = IRI("http://www.wikidata.org/prop/"),
    propStatementIri = IRI("http://www.wikidata.org/prop/statement/"),
    propQualifierIri = IRI("http://www.wikidata.org/prop/qualifier/"),
    entityAlias = "wd",
    directPropertyAlias = "wdt",
    propAlias = "p",
    propStatementAlias = "ps",
    propQualifierAlias = "pq"
  )
}
