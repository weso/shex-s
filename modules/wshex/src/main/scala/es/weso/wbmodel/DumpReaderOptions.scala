package es.weso.wbmodel
import fs2._
import fs2.io._
import fs2.compression._
import java.nio.file.Path
import org.wikidata.wdtk.datamodel.interfaces.ItemDocument
import cats.effect._
import java.io.InputStream
import java.io.OutputStream
import fs2.io.file.Files
import com.fasterxml.jackson.databind.JsonDeserializer
import org.wikidata.wdtk.datamodel.helpers

case class DumpReaderOptions(
    chunkSize: Int,
    decompressInput: Boolean,
    maxConcurrent: Int,
    site: String,
    verbose: Boolean
) {
  val jsonDeserializer = new helpers.JsonDeserializer(site)

  def withChunkSize(n: Int): DumpReaderOptions = this.copy(chunkSize = n)
  def withDecompressInput(di: Boolean): DumpReaderOptions = this.copy(decompressInput = di)
  def withMaxConcurrent(n: Int): DumpReaderOptions = this.copy(maxConcurrent = n)
  def withSite(site: String): DumpReaderOptions = this.copy(site = site)
  def withVerbose(v: Boolean): DumpReaderOptions = this.copy(verbose = v)
}

object DumpReaderOptions {

  /** Default dump options
    *
    * @return
    */
  def default: DumpReaderOptions =
    DumpReaderOptions(
      chunkSize = 4096,
      decompressInput = true,
      maxConcurrent = 200,
      site = "http://www.wikidata.org/entity/",
      verbose = false
    )
}
