package es.weso.wbmodel

import es.weso.rdf.nodes._

object Utils {

  def splitIri(iri: IRI): (String, String) = {
    val iriStr    = iri.getLexicalForm
    val separator = iriStr.lastIndexOf('/') + 1;
    try {
      (iriStr.substring(separator), iriStr.substring(0, separator))
    } catch {
      case e: IllegalArgumentException =>
        throw new IllegalArgumentException("Invalid Wikibase entity IRI: " + iriStr, e)
    }
  }

}
