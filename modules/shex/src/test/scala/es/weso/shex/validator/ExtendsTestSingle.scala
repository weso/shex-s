package es.weso.shex.validator
import es.weso.utils.VerboseLevel._

class ExtendsTestSingle extends ShouldValidateShapeMap {

  {
    val rdf =
      """|prefix : <http://e#>
           |:ok1 :q 3 .
           |:ko1 :q 99 .
           |""".stripMargin
    val shex =
      """|prefix : <http://e#>
           |:A { :q [ 3] }
           |:B { :q . }
           |:C @:A AND @:B
           |:D extends @:C { }
           |""".stripMargin
    // shouldValidateWithShapeMap(rdf, shex, ":ok1@:A", ":ok1@:A")
    shouldValidateWithShapeMap(rdf, shex, ":ko1@:A", ":ko1@!:A", Debug)
  }

}
