package es.weso.shexs
import es.weso.shex._
import cats.effect._
import cats.effect.unsafe.implicits.global
import scala.util._

/**
 * Validator wrapper is an auxiliary class than can be used to invoke the ShEx validator from Java
 * It avoids the use of IO and unsafeRunSync 
 **/
case class ValidatorWrapper private(schema: Option[Schema] = None) {
}

object ValidatorWrapper {
  def create(schemaStr: String, format: String): ValidatorWrapper = {
    val cmp: IO[Schema] = for {
      schema <- Schema.fromString(schemaStr, format, None, None)
    } yield schema
    Try(ValidatorWrapper(Some(cmp.unsafeRunSync()))) match {
     case Success(v) => v
     case Failure(exc) => throw(exc)
    }
  }
}