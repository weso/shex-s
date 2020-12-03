package es.weso.shex.validator
import es.weso.rdf.nodes.IRI
import es.weso.shex.{Annotation, Schema, ShapeExpr, ShapeLabel}
import cats.effect.IO

sealed abstract class ExternalResolver {

  def getShapeExpr(label: ShapeLabel,
                   as: Option[List[Annotation]]
                  ): IO[ShapeExpr]
}

// TODO: Should we have a list of IRIs instead of a single one?
case class ExternalIRIResolver(maybeIri: Option[IRI]) extends ExternalResolver {

  lazy val ioSchema: IO[Schema] = maybeIri match {
    case None => IO.raiseError(new RuntimeException(s"No IRI provided for ExternalIRI resolver"))
    case Some(iri) => Schema.fromIRI(iri,maybeIri)
  }

  override def getShapeExpr(label: ShapeLabel,
                            as: Option[List[Annotation]]
                           ): IO[ShapeExpr] = for {
   schema <- ioSchema
   se <- schema.getShape(label).fold(e => IO.raiseError(new RuntimeException(e)), IO(_))
  } yield se

}

/**
  * Basic external resolver that does nothing by default
  * 
  * 
  */
case object NoAction extends ExternalResolver {

  override def getShapeExpr(label: ShapeLabel,
                            as: Option[List[Annotation]]
                           ): IO[ShapeExpr] =
    IO.raiseError(new RuntimeException(s"NoAction resolver can't obtain external shape expression from label: $label"))

  /* Hack to get the instance from Java
     See: https://stackoverflow.com/questions/2561415/how-do-i-get-a-scala-case-object-from-java
   */  
  def instance: ExternalResolver = this;  
}