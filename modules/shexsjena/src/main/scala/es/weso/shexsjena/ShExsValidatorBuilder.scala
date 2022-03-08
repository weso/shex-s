package es.weso.shex.validator

import es.weso.shex.Schema
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import java.io.InputStream

object ShExsValidatorBuilder {

  def fromStringSync(schemaStr: String, format: String = "SHEXC"): ShExsValidator = {
    val cmp: IO[ShExsValidator] = for {
      schema <- Schema.fromString(schemaStr, format, None, None)
    } yield ShExsValidator(schema)
    cmp.unsafeRunSync()
  }

  def fromInputStreamSync(is: InputStream, format: String = "SHEXC"): ShExsValidator = {
    val cmp: IO[ShExsValidator] = for {
      schema <- Schema.fromInputStream(is, format, None, None)
    } yield ShExsValidator(schema)
    cmp.unsafeRunSync()
  }

}