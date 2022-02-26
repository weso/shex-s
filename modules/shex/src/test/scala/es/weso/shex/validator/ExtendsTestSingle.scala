package es.weso.shex.validator
import es.weso.utils.VerboseLevel._

class ExtendsTestSingle extends ShouldValidateShapeMap {

  {
    val rdf =
      """|prefix : <http://e#>
           |:ok1 :p 1; :q 2 .
           |:ko1 :p 1; :q 3 .
           |:ko2 :p 2; :q 2 .
           |""".stripMargin
    val shex =
      """|prefix : <http://e#>
           |:B CLOSED { :p [ 1 ] }
           |:A extends @:B {
           | :q [ 2 ]
           |}""".stripMargin
    shouldValidateWithShapeMap(rdf, shex, ":ok1@:A", ":ok1@:A", Debug)
//      shouldValidateWithShapeMap(rdf, shex, ":ko1@:A", ":ko1@!:A")
//      shouldValidateWithShapeMap(rdf, shex, ":ko2@:A", ":ko2@!:A")
  }

}
