package es.weso.shexs

import com.monovore.decline.Opts
import cats.data._
import cats.implicits._
import cats.effect.IO
import es.weso.shex.validator.Validator
import es.weso.shex.ResolvedSchema
import es.weso.shex.validator.ExternalResolver
import es.weso.rdf.RDFBuilder
import es.weso.shex.validator.Validator21
import es.weso.shex.validator.ValidatorEitherT
import es.weso.shex.validator.validatorref.ValidatorRef

sealed abstract class ValidatorVersion {
  val names: List[String]
  def buildValidator(schema: ResolvedSchema, externalResolver: ExternalResolver, builder: RDFBuilder): Validator
}

object ValidatorVersion {

  case object Version21 extends ValidatorVersion {
    val names = List("2.1")
    override def buildValidator(
        schema: ResolvedSchema,
        externalResolver: ExternalResolver,
        builder: RDFBuilder
    ): Validator =
      Validator21(schema, externalResolver, builder)

  }

  case object Version22 extends ValidatorVersion {
    val names = List("2.2", "eitherT")
    override def buildValidator(
        schema: ResolvedSchema,
        externalResolver: ExternalResolver,
        builder: RDFBuilder
    ): Validator =
      ValidatorEitherT(schema, externalResolver, builder)
  }

  case object VersionRef extends ValidatorVersion {
    val names = List("ref")
    override def buildValidator(
        schema: ResolvedSchema,
        externalResolver: ExternalResolver,
        builder: RDFBuilder
    ): Validator =
      ValidatorRef(schema, externalResolver, builder)
  }

  lazy val versions         = List(Version22, Version21, VersionRef)
  lazy val defaultVersion   = versions.head
  lazy val versionsStr      = versions.map(_.names).flatten.mkString(",")
  lazy val otherVersionsStr = versions.tail.map(_.names).flatten.mkString(",")

  lazy val validatorVersion: Opts[ValidatorVersion] =
    Opts
      .option[String](
        "validator version",
        short = "e",
        help = s"version of validator. Default = ${defaultVersion.names.head}. Other values = ${otherVersionsStr}"
      )
      .mapValidated(s =>
        versions.find(_.names.map(_.toLowerCase).contains(s.toLowerCase)) match {
          case None =>
            Validated.invalidNel[String, ValidatorVersion](
              s"Error obtaining validator version. Available values = ${versionsStr}"
            )
          case Some(vv) => Validated.validNel[String, ValidatorVersion](vv)
        }
      )
      .withDefault(defaultVersion)

}
