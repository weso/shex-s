package es.weso.shexs

import cats.effect._
import cats.implicits._
import es.weso.rdf._
import es.weso.rdf.jena.RDFAsJenaModel
import es.weso.shapemaps.ShapeMap
import es.weso.shex.{ResolvedSchema, Schema}
import es.weso.shex.validator.Validator
import es.weso.shextest.manifest.{Validate => _, _}
import com.monovore.decline._
import com.monovore.decline.effect._
// import buildinfo._
import java.nio.file.Path
import es.weso.shapepath.schemamappings.SchemaMappings
import es.weso.shex.implicits.showShEx._
import es.weso.shapepath.ProcessingError
import es.weso.shapepath.ShapePath
import es.weso.shapemaps.ResultShapeMap
import java.net.URI
import scala.util.Try
import cats.data.Validated
import es.weso.rdf.jena.Endpoint
import es.weso.rdf.nodes.IRI
import es.weso.wikibaserdf.WikibaseRDF
import es.weso.utils.FileUtils._
import es.weso.utils.VerboseLevel

// Commands  
case class SchemaMapping(
    schemaSpec: SchemaSpec, 
    mapping: Path, 
    output: Option[Path], 
    verbose: VerboseLevel)
case class ShapePathEval(
    schemaSpec: SchemaSpec, 
    shapePath: String, 
    output: Option[Path], 
    verbose: VerboseLevel)
  


object Main extends CommandIOApp (
  name="shex-s", 
  header = "ShEx-Scala command line tool",
  version = "0.2.2" // BuildInfo.version
  ) {

lazy val mappingOpt = Opts.option[Path]("mapping", short = "m", metavar = "mappings-file", help = "Path to Mappings file.")
lazy val shapePathOpt = Opts.option[String]("shapePath", help = s"ShapePath to validate a schema")
  
  
lazy val schemaMappingCommand: Opts[SchemaMapping] = 
    Opts.subcommand("mapping", "Convert a schema through a mapping") {
      (SchemaSpec.schemaSpec, mappingOpt, OutputOpt.outputOpt, VerboseLevelOpt.verboseLevel).mapN(SchemaMapping.apply)
    }


lazy val shapePathValidateCommand: Opts[ShapePathEval] =
    Opts.subcommand("shapePath","Validate a shape path") {
      (SchemaSpec.schemaSpec, shapePathOpt, OutputOpt.outputOpt, VerboseLevelOpt.verboseLevel)
      .mapN(ShapePathEval.apply)
    }



def info(msg: String, verbose: Boolean): IO[Unit] = 
   if (verbose) IO.println(msg)
   else IO(())

override def main: Opts[IO[ExitCode]] =
   (schemaMappingCommand orElse 
    Validate.validateCommand orElse
    shapePathValidateCommand orElse 
    Manifest.manifestCommand orElse
    WikibaseValidate.wikibaseCommand orElse 
    SchemaCommand.schemaCommand
   ).map {
     case smc: SchemaMapping => doSchemaMapping(smc) 
     case vc : Validate => vc.run()
     case spc: ShapePathEval => doShapePathEval(spc)
     case mf: Manifest => mf.run()
     case wc: WikibaseValidate => wc.run()
     case sc: SchemaCommand => sc.run()
   }.map(
     _.handleErrorWith(infoError)
   ) 

def infoError(err: Throwable): IO[ExitCode] =
    IO.println(s"Error ${err.getLocalizedMessage()}") *> IO(ExitCode.Error)

def doSchemaMapping(smc: SchemaMapping): IO[ExitCode] = for {
       schema <- smc.schemaSpec.getSchema(smc.verbose)
       mappingStr <- getContents(smc.mapping)
       mapping <- IO.fromEither(SchemaMappings
        .fromString(mappingStr.toString)
        .leftMap(err => new RuntimeException(s"Error parsing schema mappings: ${err}"))
        )
       newSchema <- mapping.convert(schema).fold(
         err => IO.raiseError(new RuntimeException(err.map(_.toString).mkString("\n"))),
         s => s.pure[IO],
         (warnings: List[ProcessingError], s: Schema) => for {
           _ <- IO.println(warnings.map(_.toString).mkString("\n"))
         } yield s
       )
       _ <- smc.output match {
         case None => IO.println(newSchema.show)
         case Some(outputPath) => for { 
           _ <- writeFile(outputPath.toFile().getAbsolutePath(), newSchema.show)
           _ <- IO.println(s"Output saved in ${outputPath}")
         } yield ()  
       } 
     } yield ExitCode.Success



def doShapePathEval(spc: ShapePathEval): IO[ExitCode] = for {
     schema <- spc.schemaSpec.getSchema(spc.verbose)
     shapePath <- IO.fromEither(ShapePath.fromString(spc.shapePath, "Compact", None, schema.prefixMap).leftMap(err => new RuntimeException(s"Error parsing shapePath: ${err}")))
     result <- { 
       val (ls,v) = ShapePath.eval(shapePath,schema)
       IO.println(ls.map(_.toString).mkString("\n")) *>
       v.pure[IO]
     } 
   } yield ExitCode.Success 


}
