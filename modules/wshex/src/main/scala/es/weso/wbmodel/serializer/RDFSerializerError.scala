package es.weso.wbmodel.serializer
import es.weso.wbmodel._
import org.wikidata.wdtk.datamodel.interfaces._

sealed abstract class RDFSerializerError(msg: String) extends RuntimeException(msg)
case class RDFSerializerErrorUnknownEntity(ed: EntityDoc)
    extends RDFSerializerError(s"Unexpected entitydoc: $ed. Should be item or property")
case class RDFSerializerErrorUnknownEntityIdValue(ed: EntityIdValue)
    extends RDFSerializerError(
      s"Unexpected entityIdValue: $ed. with class name: ${ed.getClass.getCanonicalName()}"
    )
