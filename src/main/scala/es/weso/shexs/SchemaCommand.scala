package es.weso.shexs

import es.weso.utils.VerboseLevel
import cats.effect.IO
import cats.effect.ExitCode
import com.monovore.decline.Opts
import cats.implicits._
import es.weso.shex.ResolvedSchema

case class SchemaCommand(
  schemaSpec: SchemaSpec,
  action: SchemaAction,
  verbose: VerboseLevel,
) {
  def run(): IO[ExitCode] = for {
    schema <- schemaSpec.getSchema
    resolved <- ResolvedSchema.resolve(schema, schemaSpec.baseIRI)
    inheritanceStr <- resolved.inheritanceGraph.show(_.toRDFNode.show)
    _ <- IO.println(s"""|Inheritance: 
                        |${inheritanceStr}
                        |""".stripMargin)
  } yield ExitCode.Success

}   

object SchemaCommand {

  val schemaCommand: Opts[SchemaCommand] = 
    Opts.subcommand("schema", "Schema processing actions") {
      (SchemaSpec.schemaSpec, SchemaAction.schemaAction, VerboseLevelOpt.verboseLevel)
      .mapN(SchemaCommand.apply)
    }

}
