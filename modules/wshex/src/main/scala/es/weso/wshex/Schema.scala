package es.weso.wshex

import es.weso.collection.Bag
import es.weso.rdf.PrefixMap
import cats.implicits._
import java.nio.file.Path
import cats.effect.IO
import es.weso.wbmodel._

case class Schema(
  shapesMap: Map[ShapeLabel, ShapeExpr],
  start: Option[ShapeExpr] = None,
  pm: PrefixMap = PrefixMap.empty
  ) extends Serializable {

 def get(shapeLabel: ShapeLabel): Option[ShapeExpr] = shapeLabel match {
   case Start => start 
   case _ => shapesMap.get(shapeLabel)
 }

 def checkLocal(label: ShapeLabel, entity: Entity): Either[Reason, Set[ShapeLabel]] = {
   get(label) match {
     case None => Left(ShapeNotFound(label,this))
     case Some(se) => se.checkLocal(entity, label, this)
   }
 }

 def checkLocalCoded(label: ShapeLabel, entity: Entity): Either[ReasonCode, Set[ShapeLabel]] = {
   get(label) match {
     case None => Left(Reason.shapeNotFound)
     case Some(se) => se.checkLocalCoded(entity, label, this)
   }
 }

 def checkNeighs(
   label: ShapeLabel, 
   neighs: Bag[(PropertyId,ShapeLabel)],
   failed: Set[(PropertyId, ShapeLabel)]
   ): Either[Reason, Unit] = {
   get(label) match {
     case None => Left(ShapeNotFound(label,this))
     case Some(se) => se.checkNeighs(neighs, failed, this)
   }
 }

 def checkNeighsCoded(
   label: ShapeLabel, 
   neighs: Bag[(PropertyId,ShapeLabel)],
   failed: Set[(PropertyId, ShapeLabel)]
   ): Either[ReasonCode, Unit] = {
   get(label) match {
     case None => Left(Reason.shapeNotFound)
     case Some(se) => se.checkNeighsCoded(neighs, failed, this)
   }
 }

 def getTripleConstraints(label: ShapeLabel): List[(PropertyId, ShapeLabel)] = {
   get(label) match {
     case None => List()
     case Some(se) => {
      val tcs = se.tripleConstraints(this).map(tc => 
        (tc.property, tc.value.label)
      )
     /* println(s"""|TripleConstraints($label)=
                  |${tcs.mkString("\n")}
                  |---end TripleConstraints($label)
                  |""".stripMargin) */
      tcs
     }
   }
 }

 lazy val shapes: List[ShapeExpr] = {
    shapesMap.values.toList
 }


  /**
   * Get a shape with label 
   *
   * @param lbl
   * @return the shape expression with that label
   */ 
 def getShape(lbl: ShapeLabel): Option[ShapeExpr] =
    shapesMap.get(lbl)


  /**
   * Start shape expression in a schema
   *
   * @return the start shape expression if it has been declared or the first one. None if there are no shape expressions 
   */   
  lazy val startShapeExpr: Option[ShapeExpr] = 
    getShape(Start).orElse(shapes.headOption)   

}

object Schema {

 private def cnvFormat(format: WShExFormat): String = format match {
        case CompactFormat => "ShEXC"
        case JSONFormat => "JSON"
    }

    
  def fromPath(
   path: Path, 
   format: WShExFormat = CompactFormat
   ): IO[Schema] = for {
    schema <- es.weso.shex.Schema.fromFile(path.toFile().getAbsolutePath(), cnvFormat(format))
    resolvedSchema <- es.weso.shex.ResolvedSchema.resolve(schema, None)
    schema <- IO.fromEither(ShEx2WShEx().convertSchema(resolvedSchema))
  } yield schema

  def fromString(
                schemaString: String,
                format: WShExFormat = CompactFormat
              ): IO[Schema] = for {
    schema <- es.weso.shex.Schema.fromString(schemaString, cnvFormat(format))
    resolvedSchema <- es.weso.shex.ResolvedSchema.resolve(schema, None)
    schema <- IO.fromEither(ShEx2WShEx().convertSchema(resolvedSchema))
  } yield schema

 /**
   * Read a Schema from a file
   * This version is unsafe in the sense that it can throw exceptions
   * Use `fromPath` for a safe version which returns an `IO[Schema]`
   *
   * @param path file to read
   * @param format it can be CompactFormat or JsonFormat
   * @return the schema
   */ 
 def unsafeFromPath(
   path: Path, 
   format: WShExFormat = CompactFormat
   ): Schema = {
    import cats.effect.unsafe.implicits.global
    fromPath(path, format).unsafeRunSync()
  }

    /**
    * Read a Schema from a file
    * This version is unsafe in the sense that it can throw exceptions
    * Use `fromPath` for a safe version which returns an `IO[Schema]`
    *
    * @param str string that represents the schema
    * @param format it can be CompactFormat or JsonFormat
    * @return the schema
    */
   def unsafeFromString(str: String, format: WShExFormat): Either[ParseError, Schema] = {
        import cats.effect.unsafe.implicits.global
        try {
          val schema = es.weso.shex.Schema.fromString(str,cnvFormat(format)).unsafeRunSync()
          val wShEx = ShEx2WShEx().convertSchema(schema)
          wShEx.bimap(ConversionError(_), identity)
        } catch {
            case e: Exception => ParseException(e).asLeft
        }
    }


  /**
    * Read a Schema from a file
    * This version is unsafe in the sense that it can throw exceptions
    * Use `fromPath` for a safe version which returns an `IO[Schema]`
    *
    * @param str String that represents the schema
    * @param format it can be CompactFormat or JsonFormat
    * @return the schema
    */
  def unsafeFromString2(
                      schemaString: String,
                      format: WShExFormat = CompactFormat
                    ): Schema = {
    import cats.effect.unsafe.implicits.global
    fromString(schemaString, format).unsafeRunSync()
  }
}
