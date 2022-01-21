package es.weso.shextest.manifest

import es.weso.rdf.nodes._
import java.nio.file.Path
import cats.effect.IO
import es.weso.utils.testsuite.TestSuite
import java.net.URI
import es.weso.utils._


case class ShExManifest(
  label: Option[String],
  comment: Option[String],
  entries: List[Entry],
  includes: List[(RDFNode, Option[ShExManifest])]) {

  def toTestSuite(uri: URI, verbose: VerboseLevel): TestSuite = 
   TestSuite(entries.map(_.toTestEntry(uri, verbose)))  
}

object ShExManifest {

  def empty: ShExManifest = 
    ShExManifest(
      None, 
      None, 
      List(), 
      List()
  )

  def fromPath(path: Path, format: String, base: Option[String], derefIncludes: Boolean): IO[ShExManifest] = 
   RDF2Manifest.read(path,format,base,derefIncludes)
}



