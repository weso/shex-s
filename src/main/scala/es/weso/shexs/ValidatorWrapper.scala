package es.weso.shexs
import es.weso.shex._
import cats.effect._
import cats.effect.unsafe.implicits.global
import scala.util._
import es.weso.shapemaps.ResultShapeMap
import es.weso.rdf.jena.RDFAsJenaModel
import org.apache.jena.rdf.model.Model
import org.apache.jena.rdf.model.RDFNode

/**
 * Validator wrapper is an auxiliary class than can be used to invoke the ShEx validator from Java
 * It avoids the use of IO and unsafeRunSync 
 **/
case class ValidatorWrapper(schema: Option[AbstractSchema] = None) {
  
  def this() = {
    this(None)
  }

  def parseSchema(schemaStr: String, format: String): ValidatorWrapper = {
    val cmp: IO[ResolvedSchema] = for {
      schema <- Schema.fromString(schemaStr, format, None, None)
      resolvedSchema <- ResolvedSchema.resolve(schema, None)
    } yield resolvedSchema

    ValidatorWrapper(Some(cmp.unsafeRunSync()))
  }


  def validate(dataModel: Model): ResultShapeMap = {
    val cmp: IO[ResultShapeMap] = for {
      emptyRes <- RDFAsJenaModel.empty
    } yield ResultShapeMap.empty
    ???
  } 
}

