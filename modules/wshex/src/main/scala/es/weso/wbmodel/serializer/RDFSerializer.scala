package es.weso.wbmodel.serializer

import cats.effect._
import cats.implicits._
import es.weso.wbmodel.EntityDoc
import cats.implicits._
import org.wikidata.wdtk.rdf.{PropertyRegister, RdfSerializer}
import java.io.OutputStream
import org.eclipse.rdf4j.rio.RDFFormat
import org.wikidata.wdtk.rdf.RdfConverter
import org.wikidata.wdtk.rdf.RdfWriter
import org.wikidata.wdtk.datamodel.implementation.SitesImpl
import org.wikidata.wdtk.rdf.PropertyRegister
import org.wikidata.wdtk.datamodel.interfaces.ItemDocument
import org.wikidata.wdtk.datamodel.interfaces.PropertyDocument
import java.net.URI
import java.io.ByteArrayOutputStream
import es.weso.rdf.jena._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rdf.triples._
import org.wikidata.wdtk.datamodel.interfaces.EntityDocument
import org.wikidata.wdtk.datamodel.interfaces.TermedDocument
import org.wikidata.wdtk.datamodel.interfaces.StatementDocument
import org.wikidata.wdtk.datamodel.interfaces.MonolingualTextValue
import scala.jdk.CollectionConverters._
import org.wikidata.wdtk.datamodel.interfaces._
import org.wikidata.wdtk.rdf.Vocabulary
import org.wikidata.wdtk.rdf.values.TimeValueConverter
import es.weso.rdf.PREFIXES._
import es.weso.rdf.saver.RDFSaver

// TODO: Rewrite this code using RDFSaver instead of plain RDFBuilder methods...
// TODO: Change format from String to RDFFormat

case class RDFSerializer(format: String) extends Serializer with RDFSaver {

  // val logger = Logger

  /* I would prefer to reuse the definitions from Wikidata-toolkit
     https://github.com/Wikidata/Wikidata-Toolkit/blob/master/wdtk-rdf/src/main/java/org/wikidata/wdtk/rdf/AbstractRdfConverter.java
    However, that code is based on OutputStreams which make the code imperative
    so we decided to replicate those definitions using SRDF
   */

  private val wd = IRI("http://www.wikidata.org/entity/")
  private val geo = IRI(Vocabulary.PREFIX_GEO)
  private val wdt = IRI(Vocabulary.PREFIX_PROPERTY_DIRECT)
  private val rdf = IRI(Vocabulary.PREFIX_RDF)
  private val p = IRI(Vocabulary.PREFIX_PROPERTY)
  private val ps = IRI(Vocabulary.PREFIX_PROPERTY_STATEMENT)
  private val pq = IRI(Vocabulary.PREFIX_PROPERTY_QUALIFIER)
  private val pr = IRI(Vocabulary.PREFIX_PROPERTY_REFERENCE)
  private val prov = IRI(Vocabulary.PREFIX_PROV)
  private val rdfs = IRI(Vocabulary.PREFIX_RDFS)
  private val wds = IRI(Vocabulary.PREFIX_WIKIDATA_STATEMENT)
  private val skos = IRI(Vocabulary.PREFIX_SKOS)
  private val schema = IRI(Vocabulary.PREFIX_SCHEMA)
  private val wikibase = IRI(Vocabulary.PREFIX_WBONTO)
  private val owl = IRI(Vocabulary.PREFIX_OWL)
  private val xsd = IRI(Vocabulary.PREFIX_XSD)
  private val wdno = IRI("http://www.wikidata.org/prop/novalue/")

  private val rdfs_label = rdfs + "label"
  private val skos_prefLabel = skos + "prefLabel"
  private val skos_altLabel = skos + "altLabel"
  private val wikibase_Item = wikibase + "Item"
  private val schema_description = schema + "description"
  private val xsd_string = xsd + "string"
  private val prov_wasDerivedFrom = prov + "wasDerivedFrom"

  private val wikibasePrefixMap = PrefixMap(
    Map(
      Prefix("geo") -> geo,
      Prefix("p") -> p,
      Prefix("ps") -> ps,
      Prefix("pq") -> pq,
      Prefix("pr") -> pr,
      Prefix("prov") -> prov,
      Prefix("rdf") -> rdf,
      Prefix("rdfs") -> rdfs,
      Prefix("wds") -> wds,
      Prefix("skos") -> skos,
      Prefix("schema") -> schema,
      Prefix("wd") -> wd,
      Prefix("wdt") -> wdt,
      Prefix("wikibase") -> wikibase,
      Prefix("xsd") -> xsd,
      Prefix("wdno") -> wdno
    )
  )

  private def prefixDecls: IO[String] =
    RDFAsJenaModel.empty.flatMap(
      _.use(rdf =>
        for {
          _ <- rdf.addPrefixMap(wikibasePrefixMap)
          str <- rdf.serialize("TURTLE")
        } yield str
      )
    )

  private def mkLangString(str: MonolingualTextValue): LangLiteral =
    LangLiteral(str.getText(), Lang(str.getLanguageCode()))

  private def mkTerms(
      subj: IRI,
      prop: IRI,
      ts: Map[String, MonolingualTextValue],
      rdf: RDFBuilder
  ): IO[RDFBuilder] =
    ts.toList.map(_._2).foldM(rdf) { case (current, v) =>
      current.addTriple(RDFTriple(subj, prop, mkLangString(v)))
    }

  private def mkTermsLs(
      subj: IRI,
      prop: IRI,
      ts: java.util.Map[String, java.util.List[MonolingualTextValue]],
      rdf: RDFBuilder
  ): IO[RDFBuilder] =
    ts.asScala.toList.map(_._2).map(_.asScala).flatten.foldM[IO, RDFBuilder](rdf) {
      case (current, v) => current.addTriple(RDFTriple(subj, prop, mkLangString(v)))
    }

  private def mkEntityDocument(e: EntityDocument, rdf: RDFBuilder): IO[RDFBuilder] =
    e match {
      case id: ItemDocument     => mkItemDocument(id, rdf)
      case pd: PropertyDocument => mkPropertyDocument(pd, rdf)
    }

  private def mkLabels(td: TermedDocument, rdf: RDFBuilder): IO[RDFBuilder] =
    mkTerms(mkEntity(td), rdfs_label, td.getLabels().asScala.toMap, rdf)

  private def mkDescriptions(td: TermedDocument, rdf: RDFBuilder): IO[RDFBuilder] =
    mkTerms(mkEntity(td), schema_description, td.getDescriptions().asScala.toMap, rdf)

  private def mkAliases(td: TermedDocument, rdf: RDFBuilder): IO[RDFBuilder] =
    mkTermsLs(mkEntity(td), skos_altLabel, td.getAliases(), rdf)

  private def mkDocumentTerms(td: TermedDocument, rdf: RDFBuilder): IO[RDFBuilder] =
    mkLabels(td, rdf) *>
      mkDescriptions(td, rdf) *>
      mkAliases(td, rdf)

  private def getBestRank(sg: StatementGroup): Option[StatementRank] = {
    val bestStatements = sg.getBestStatements()
    if (bestStatements != null)
      Some(bestStatements.iterator().next.getRank)
    else
      None
  }

  private def isBest(statement: Statement, bestRank: Option[StatementRank]): Boolean = {
    val sr = statement.getRank()
    val maybeSr = if (sr == null) None else Some(sr)
    bestRank == maybeSr
  }

  private def mkSimpleStatement(statement: Statement, rdf: RDFBuilder): IO[RDFBuilder] = {
    val subj = IRI(statement.getSubject().getIri())
    val pred = statement.getMainSnak().getPropertyId()
    val snakRdfConverter = SnakRdfConverter(subj, pred, rdf, Direct(wdt))
    statement.getMainSnak().accept(snakRdfConverter)
  }

  private case class SnakRdfConverter(
      subj: RDFNode,
      predId: PropertyIdValue,
      rdf: RDFBuilder,
      mode: Mode
  ) extends SnakVisitor[IO[RDFBuilder]]
      with ValueVisitor[IO[RDFBuilder]] {

    val pred = mode.base + predId.getId()
    override def visit(noValue: NoValueSnak): IO[RDFBuilder] =
      rdf.addTriple(RDFTriple(subj, `rdf:type`, wdno + predId.getId()))

    override def visit(someValue: SomeValueSnak): IO[RDFBuilder] =
      rdf.createBNode.flatMap { pair =>
        val (bnode, newRdf) = pair
        newRdf.addTriple(RDFTriple(subj, pred, bnode))
      }

    override def visit(value: ValueSnak): IO[RDFBuilder] = {
      val v = value.getValue
      v.accept(this)
    }

    override def visit(value: GlobeCoordinatesValue): IO[RDFBuilder] =
      rdf.addTriple(RDFTriple(subj, pred, GlobeCoordinatesConverter.getLiteral(value)))
    // TODO add complex representation...

    override def visit(value: EntityIdValue): IO[RDFBuilder] = value match {
      case id: ItemIdValue     => rdf.addTriple(RDFTriple(subj, pred, IRI(id.getIri())))
      case id: PropertyIdValue => rdf.addTriple(RDFTriple(subj, pred, IRI(id.getIri())))
      case fd: FormIdValue     => rdf.addTriple(RDFTriple(subj, pred, IRI(fd.getIri())))
      case lv: LexemeIdValue   => rdf.addTriple(RDFTriple(subj, pred, IRI(lv.getIri())))
      case sv: SenseIdValue    => rdf.addTriple(RDFTriple(subj, pred, IRI(sv.getIri())))
      case _ =>
        IO.println(s"Unexpected entityIdValue: $value") *>
          rdf.addTriple(RDFTriple(subj, pred, IRI(value.getIri())))
    }
    override def visit(value: MonolingualTextValue): IO[RDFBuilder] =
      rdf.addTriple(RDFTriple(subj, pred, mkLangString(value)))

    override def visit(value: QuantityValue): IO[RDFBuilder] =
      rdf.addTriple(RDFTriple(subj, pred, QuantityConverter.getQuantityLiteral(value)))

    override def visit(x: StringValue): IO[RDFBuilder] =
      rdf.addTriple(RDFTriple(subj, pred, StringLiteral(x.getString())))

    override def visit(timeValue: TimeValue): IO[RDFBuilder] = {
      val literal = TimeConverter.getTimeLiteral(timeValue)
      rdf.addTriple(RDFTriple(subj, pred, literal))
      // TODO: Add complex representation of time values
    }

    override def visit(x: UnsupportedValue): IO[RDFBuilder] =
      rdf.addTriple(RDFTriple(subj, pred, StringLiteral(x.getTypeJsonString())))

  }

  private def mkFullStatement(statement: Statement, rdf: RDFBuilder): IO[RDFBuilder] = {
    val iriStatement = IRI(Vocabulary.getStatementUri(statement))
    val subj = IRI(statement.getSubject().getIri())
    val propId = statement.getMainSnak().getPropertyId()
    val pred = p + propId.getId()
    rdf.addTriple(RDFTriple(subj, pred, iriStatement)) *>
      rdf.addType(iriStatement, IRI(Vocabulary.WB_STATEMENT)) *>
      mkClaim(iriStatement, propId, statement.getClaim(), rdf) *>
      mkReferences(iriStatement, propId, statement.getReferences().asScala.toList, rdf)
    // TODO: references, sitelinks
  }

  private def mkClaim(
      subj: IRI,
      propId: PropertyIdValue,
      claim: Claim,
      rdf: RDFBuilder
  ): IO[RDFBuilder] = {
    val snakRdfConverter = SnakRdfConverter(subj, propId, rdf, PropertyStatement(ps))
    claim.getMainSnak().accept(snakRdfConverter) *>
      claim.getAllQualifiers().asScala.toList.foldM(rdf) { case (current, snak) =>
        val qualifierConverter =
          SnakRdfConverter(subj, snak.getPropertyId(), current, Qualifier(pq))
        snak.accept(qualifierConverter)
      }
  }

  private def mkReferences(
      subj: IRI,
      propId: PropertyIdValue,
      references: List[Reference],
      rdf: RDFBuilder
  ): IO[RDFBuilder] =
    references.foldM(rdf) { case (currentRdf, ref) =>
      mkReference(subj, propId, ref, currentRdf)
    }

  private def mkReference(
      subj: IRI,
      propId: PropertyIdValue,
      reference: Reference,
      rdf: RDFBuilder
  ): IO[RDFBuilder] = for {
    pair <- rdf.createBNode
    // (bnode, rdf1): (RDFNode, RDFBuilder) = pair
    bnode = pair._1
    rdf1 = pair._2
    rdf2 <- rdf1.addTriple(RDFTriple(subj, prov_wasDerivedFrom, bnode))
    rdf3 <- reference.getAllSnaks().asScala.toList.foldM[IO, RDFBuilder](rdf2)(convertSnak(bnode))
  } yield rdf3

  private def convertSnak(bnode: RDFNode)(current: RDFBuilder, snak: Snak): IO[RDFBuilder] = {
    val referenceConverter: SnakVisitor[IO[RDFBuilder]] =
      SnakRdfConverter(bnode, snak.getPropertyId(), current, ReferenceMode(pr))
    val x: IO[RDFBuilder] = snak.accept(referenceConverter)
    x
  }

  private def mkStatement(
      statement: Statement,
      rdf: RDFBuilder,
      bestRank: Option[StatementRank]
  ): IO[RDFBuilder] =
    if (isBest(statement, bestRank)) {
      mkSimpleStatement(statement, rdf) *>
        mkFullStatement(statement, rdf)
    } else
      mkFullStatement(statement, rdf)

  private def mkStatementGroup(
      sg: StatementGroup,
      rdf: RDFBuilder,
      bestRank: Option[StatementRank]
  ): IO[RDFBuilder] =
    sg.iterator().asScala.toList.foldM(rdf) { case (current, statement) =>
      mkStatement(statement, current, bestRank)
    }

  private def mkDocumentStatements(sd: StatementDocument, rdf: RDFBuilder): IO[RDFBuilder] = {
    val statementGroup = sd.getStatementGroups().asScala.toList
    statementGroup.foldM(rdf) { case (current, sg) =>
      val bestRank = getBestRank(sg)
      mkStatementGroup(sg, current, bestRank)
    }
  }

  private def mkItemDocument(id: ItemDocument, rdf: RDFBuilder): IO[RDFBuilder] =
    rdf.addType(mkEntity(id), wikibase_Item) *>
      mkDocumentTerms(id, rdf) *>
      mkDocumentStatements(id, rdf)

  private def mkPropertyDocument(pd: PropertyDocument, rdf: RDFBuilder): IO[RDFBuilder] =
    rdf.addType(mkEntity(pd), wikibase_Item)

  /*    addTriple(
      RDFTriple(mkEntity(id) , IRI("a"), StringLiteral(e.getEntityId().getId()))
    ) */

  private def mkEntity(e: EntityDocument): IRI =
    IRI(e.getEntityId().getIri)

  def serialize(entityDocument: EntityDocument): IO[String] =
    RDFAsJenaModel.empty.flatMap(
      _.use(rdf =>
        for {
          _ <- rdf.addPrefixMap(wikibasePrefixMap)
          _ <- mkEntityDocument(entityDocument, rdf)
          str <- rdf.serialize("TURTLE")
        } yield RDFSerializer.removePrefixes(str)
      )
    )

  def start = prefixDecls

  def end = IO("")
  def sep = "\n"

}

object RDFSerializer {

  def makeSerializer(format: String): Resource[IO, RDFSerializer] = {
    def acquire: IO[RDFSerializer] = IO {
      RDFSerializer(format)
    }

    def release(r: RDFSerializer): IO[Unit] =
      ().pure[IO]

    Resource.make(acquire)(release)
  }

  private def removeLine(str: String): String =
    str.substring(str.indexOf("\n") + 1)

  private def removePrefix(str: String): Option[String] =
    if (
      str.toLowerCase.startsWith("prefix") ||
      str.toLowerCase.startsWith("@prefix")
    ) Some(removeLine(str))
    else None

  def removePrefixes(str: String): String =
    removePrefix(str) match {
      case None          => str
      case Some(removed) => removePrefixes(removed)
    }

}
