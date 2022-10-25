package es.weso.wbmodel.serializer
import es.weso.rdf.nodes._

sealed abstract class Mode {
    val base: IRI
}
case class Direct(base: IRI) extends Mode 
case class Property(base: IRI) extends Mode  
case class Qualifier(base: IRI) extends Mode 
case class ReferenceMode(base: IRI) extends Mode