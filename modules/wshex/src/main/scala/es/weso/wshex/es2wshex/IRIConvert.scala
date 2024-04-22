package es.weso.wshex.es2wshex

import es.weso.wshex.`prov:wasDerivedFrom`
import es.weso.rdf.nodes.IRI
import es.weso.wbmodel.Utils
import cats.implicits._

object IRIConvert {

  private def parseDirect(
      pred: IRI,
      convertOptions: ES2WShExConvertOptions
  ): Option[DirectProperty] = {
    val (name, base) = Utils.splitIri(pred)
    val expr = "P(\\d*)".r
    if (IRI(base) == convertOptions.directPropertyIri) {
      name match {
        case expr(num) =>
          Some(DirectProperty(Integer.parseInt(num)))
        case _ => None
      }
    } else None
  }

  private def parsePropertyStatement(
      pred: IRI,
      convertOptions: ES2WShExConvertOptions
  ): Option[PropertyStatement] = {
    val (name, base) = Utils.splitIri(pred)
    val expr = "P(\\d*)".r
    if (IRI(base) == convertOptions.propStatementIri)
      name match {
        case expr(num) => Some(PropertyStatement(Integer.parseInt(num)))
        case _         => None
      }
    else None
  }

  private def parsePropertyQualifier(
      pred: IRI,
      convertOptions: ES2WShExConvertOptions
  ): Option[PropertyQualifier] = {
    val (name, base) = Utils.splitIri(pred)
    val expr = "P(\\d*)".r
    if (IRI(base) == convertOptions.propQualifierIri)
      name match {
        case expr(num) => Some(PropertyQualifier(Integer.parseInt(num)))
        case _         => None
      }
    else None
  }

  private def parsePropertyReference(
      pred: IRI,
      convertOptions: ES2WShExConvertOptions
  ): Option[PropertyReference] = {
    val (name, base) = Utils.splitIri(pred)
    val expr = "P(\\d*)".r
    if (IRI(base) == convertOptions.propReferenceIri)
      name match {
        case expr(num) => Some(PropertyReference(Integer.parseInt(num)))
        case _         => None
      }
    else None
  }

  private def parseProperty(pred: IRI, convertOptions: ES2WShExConvertOptions): Option[Property] = {
    val (name, base) = Utils.splitIri(pred)
    val expr = "P(\\d*)".r
    if (IRI(base) == convertOptions.propIri)
      name match {
        case expr(num) => Some(Property(Integer.parseInt(num)))
        case _         => None
      }
    else None
  }

  def parseIRI(iri: IRI, convertOptions: ES2WShExConvertOptions): Option[IRIParsed] =
    parseDirect(iri, convertOptions)
      .orElse(parseProperty(iri, convertOptions))
      .orElse(parsePropertyStatement(iri, convertOptions))
      .orElse(parsePropertyQualifier(iri, convertOptions))
      .orElse(parsePropertyReference(iri, convertOptions))
      .orElse(parseWasDerivedFrom(iri))

  def parseWasDerivedFrom(iri: IRI): Option[IRIParsed] =
    if (iri == `prov:wasDerivedFrom`) WasDerivedFrom.some
    else none

}
