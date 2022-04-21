package es.weso.shex.validator
import es.weso.utils.VerboseLevel._

class ExtendsTestSingle extends ShouldValidateShapeMap {

  {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { :p [ 1 ] }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p 1 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A", ":a@:A", Debug)
  }
}
