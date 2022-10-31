package es.weso.shex.validator
import es.weso.utils.VerboseLevel._

class ExtendsTestSingle extends ShouldValidateShapeMap {

  {
    val rdf =
      """|prefix : <http://e#>
         |:x :p 1, 2, 3, 4 .""".stripMargin
    val shex =
      """|prefix : <http://e#>
         |:A { :p [ 1 ] }
         |:B extends @:A { :p [ 2 ] }
         |:C extends @:A { :p [ 3 ] }
         |:D extends @:B extends @:C { :p [ 4 ] }
         |""".stripMargin
    shouldValidateWithShapeMap(rdf, shex, ":x@:D", ":x@:D", Debug)
  }


}
