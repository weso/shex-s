package es.weso.shex.validator
import es.weso.rdf.nodes.IRI
import es.weso.shex.{Annotation, Schema, ShapeExpr, ShapeLabel}
import cats.effect.IO

sealed abstract class ExternalResolver(name: String) {

  def getShapeExpr(label: ShapeLabel,
                   as: Option[List[Annotation]]
                  ): IO[ShapeExpr]
}


object ExternalResolver {

 case class NoActionException(label: ShapeLabel, as: Option[List[Annotation]]) 
   extends RuntimeException(s"""|Resolver: NoAction
                                |Can't obtain external shape from label $label
                                |Annotations: ${as.getOrElse(List()).map(_.toString).mkString(",")}""".stripMargin)  


 /**
   * Basic external resolver that does nothing by default
   */
 case object NoAction extends ExternalResolver("NoAction") {

   override def getShapeExpr(label: ShapeLabel,
                            as: Option[List[Annotation]]
                           ): IO[ShapeExpr] =
     IO.raiseError(NoActionException(label, as))

   /* Hack to get the instance from Java
      See: https://stackoverflow.com/questions/2561415/how-do-i-get-a-scala-case-object-from-java
    */  
   def instance: ExternalResolver = this;  
 }

case class ExternalIRIResolver(iri: IRI) extends ExternalResolver("ExternalIRIResolver") {

  // TODO: We should cache the schema instead of the IO action!
  lazy val ioSchema: IO[Schema] = Schema.fromIRI(iri, None)

  override def getShapeExpr(label: ShapeLabel,
                            as: Option[List[Annotation]]
                           ): IO[ShapeExpr] = for {
   schema <- ioSchema
   se <- schema.getShape(label).fold(e => IO.raiseError(ExternalIRIResolverException(label,as,iri,e)), IO(_))
  } yield se

}

 case class ExternalIRIResolverException(label: ShapeLabel, as: Option[List[Annotation]], iri: IRI, e: String) 
   extends RuntimeException(s"""|ExternalIRIResolver resolver
                                |Can't obtain external shape from label $label
                                |Error: ${e}
                                |Annotations: ${as.getOrElse(List()).map(_.toString).mkString(",")}""".stripMargin)  


}
