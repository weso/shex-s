package es.weso.shex.compact

import cats.implicits._
import es.weso.shex.Schema
<<<<<<< HEAD
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
=======
import org.scalatest._
import cats.data._ 
import cats.effect.IO
import cats.implicits._
import matchers.should._
import funspec._

>>>>>>> issue57

class CompactShowTest extends AnyFunSpec with Matchers with EitherValues {

  describe(s"Compact show test") {
   shouldShowAndParse(
     """|prefix : <http://example.org/>
       |:S IRI
     """.stripMargin)
  }

  def shouldShowAndParse(shexStr: String): Unit = {
    it(s"Should show/parse $shexStr") {
    val result: EitherT[IO,String,Boolean] = for {
      shExSchema <- EitherT.liftF(Schema.fromString(shexStr))
      serialized = CompactShow.showSchema(shExSchema)
      schemaFromSerialized <- EitherT.liftF(Schema.fromString(serialized)).leftMap((e:String) => s"Error reading serialized schema: $e\nSerialized:\n$serialized")
      b <- EitherT.fromEither[IO](shouldBeEqualSchemas(shExSchema, schemaFromSerialized).asRight)
    } yield b
    result.value.unsafeRunSync.fold(
      e => fail(s"$e"), 
      (v: Boolean) => v should be(true)
      )
    }
  }

  def shouldBeEqualSchemas(s1: Schema, s2: Schema): Boolean =
    if (s1 === s2) true
    else false
}
