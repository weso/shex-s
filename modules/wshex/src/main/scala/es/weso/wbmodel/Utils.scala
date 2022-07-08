package es.weso.wbmodel

import es.weso.rdf.nodes._

object Utils {

  /** Get local name and prefix of a IRI
    * This code has been adapted from WikidataToolkit ItemIdValueImpl
    * @param iri
    * @return a pair with (localName, base)
    */
  def splitIri(iri: IRI): (String, String) = {
    val iriStr = iri.getLexicalForm
    val separator = iriStr.lastIndexOf('/') + 1;
    try {
      val localName = iriStr.substring(separator)
      val base = iriStr.substring(0, separator)
      (localName, base)
    } catch {
      case e: IllegalArgumentException =>
        throw new IllegalArgumentException(
          s"splitIri($iriStr): Error spliting IRI into localName and base: ",
          e
        )
    }
  }

}
