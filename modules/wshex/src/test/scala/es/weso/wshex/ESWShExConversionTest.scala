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

  checkConversion("humans")
  checkConversion("humans2")

  def checkConversion(
      name: String,
      verboseLevel: VerboseLevel = VerboseLevel.Nothing
  )(implicit loc: munit.Location): Unit =
    test(name) {
      assertIO(
        for {
          wshex1 <- WSchema.fromPath(
            path = getResourcePath(name + ".shex"),
            format = WShExFormat.ESCompactFormat,
            verbose = verboseLevel
          )
          wshex2 <- WSchema.fromPath(
            path = getResourcePath(name + ".wshex"),
            format = WShExFormat.CompactWShExFormat,
            verbose = verboseLevel
          )
          _ <-
            if (wshex1.shapes != wshex2.shapes) {
              IO.println(
                s"Schemas are different\nschema1 = ${wshex1.shapes.toString}\nschema2 = ${wshex2.shapes.toString}"
              )
            } else IO.pure(())
        } yield {
          assertEquals(wshex1.shapes, wshex2.shapes)
          wshex1.shapes == wshex2.shapes
        },
        true
      )
    }

  def getResourceInputStream(fileName: String): InputStream =
    getClass().getClassLoader().getResourceAsStream(fileName)

  def getResourcePath(fileName: String): Path =
    Paths.get(getClass().getClassLoader().getResource(fileName).toURI().getPath())


  }
