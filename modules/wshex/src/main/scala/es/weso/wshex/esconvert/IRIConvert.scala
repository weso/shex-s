package es.weso.wshex.esconvert

import es.weso.rdf.nodes.IRI
import es.weso.wshex.ESConvertOptions
import es.weso.wbmodel.Utils

sealed abstract class IRIParsed
case class DirectProperty(value: Int) extends IRIParsed
case class Property(value: Int) extends IRIParsed
case class PropertyStatement(value: Int) extends IRIParsed
case class PropertyQualifier(value: Int) extends IRIParsed

object IRIConvert {

  private def parseDirect(pred: IRI, convertOptions: ESConvertOptions): Option[DirectProperty] = {
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
      convertOptions: ESConvertOptions
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
      convertOptions: ESConvertOptions
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

  private def parseProperty(pred: IRI, convertOptions: ESConvertOptions): Option[Property] = {
    val (name, base) = Utils.splitIri(pred)
    val expr = "P(\\d*)".r
    if (IRI(base) == convertOptions.propIri)
      name match {
        case expr(num) => Some(Property(Integer.parseInt(num)))
        case _         => None
      }
    else None
  }

  def parseIRI(iri: IRI, convertOptions: ESConvertOptions): Option[IRIParsed] =
    parseDirect(iri, convertOptions)
      .orElse(parsePropertyStatement(iri, convertOptions))
      .orElse(parseProperty(iri, convertOptions))
      .orElse(parsePropertyQualifier(iri, convertOptions))

}
