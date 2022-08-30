package es.weso.wshex
import es.weso.utils.FileUtils._
import munit._
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.Files
import es.weso.wbmodel.DumpReader
import es.weso.wbmodel.EntityDoc
import cats.effect.IO
import cats._
import cats.implicits._
import java.io.InputStream
import es.weso.wshex.WShExFormat
import es.weso.utils.VerboseLevel
import es.weso.wbmodel.Value
import org.wikidata.wdtk.datamodel.helpers.StatementBuilder
import org.wikidata.wdtk.datamodel.implementation.PropertyIdValueImpl
import org.wikidata.wdtk.datamodel.implementation.ItemIdValueImpl
import org.wikidata.wdtk.datamodel.interfaces.Reference
import org.wikidata.wdtk.datamodel.implementation.ReferenceImpl
import org.wikidata.wdtk.datamodel.helpers.ReferenceBuilder
import es.weso.wshex.WSchema

class ESWShExConversionTest extends CatsEffectSuite {

  def getResourceInputStream(fileName: String): InputStream =
    getClass().getClassLoader().getResourceAsStream(fileName)

  def getResourcePath(fileName: String): Path =
    Paths.get(getClass().getClassLoader().getResource(fileName).toURI().getPath())

  checkConversion("humans")

  def checkConversion(
      name: String,
      verboseLevel: VerboseLevel = VerboseLevel.Nothing
  )(implicit loc: munit.Location): Unit =
    test(name) {
      assertIO(
        for { 
          wshex1 <- WSchema.fromPath(getResourcePath(name + ".shex"), WShExFormat.ESCompactFormat, verboseLevel)
          wshex2 <- WSchema.fromPath(getResourcePath(name + ".wshex"), WShExFormat.CompactWShExFormat, verboseLevel)
          _ <- if (wshex1 != wshex2) {
            IO.println(s"Schemas are different\nschema1 = ${wshex1.toString}\nschema2 = ${wshex2.toString}")
          } else IO.pure(())
        } yield { 
          assertEquals(wshex1, wshex2) 
          wshex1 == wshex2
        }, true
      )
    }
}
