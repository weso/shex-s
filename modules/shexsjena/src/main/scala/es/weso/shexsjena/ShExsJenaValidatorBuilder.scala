package es.weso.shexsjena

import es.weso.shex.Schema
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import java.io.InputStream

object ShExsJenaValidatorBuilder {

  def fromStringSync(schemaStr: String, format: String = "SHEXC"): ShExsJenaValidator = {
    val cmp: IO[ShExsJenaValidator] = for {
      schema <- Schema.fromString(schemaStr, format, None, None)
    } yield ShExsJenaValidator(schema)
    cmp.unsafeRunSync()
  }

  def fromInputStreamSync(is: InputStream, format: String = "SHEXC"): ShExsJenaValidator = {
    val cmp: IO[ShExsJenaValidator] = for {
      schema <- Schema.fromInputStream(is, format, None, None)
    } yield ShExsJenaValidator(schema)
    cmp.unsafeRunSync()
  }

}
