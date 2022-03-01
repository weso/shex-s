package es.weso.shex.validator
import es.weso.utils.VerboseLevel._

class ExtendsTestSingle extends ShouldValidateShapeMap {

  {
    val rdf =
      """|prefix : <http://e#>
           |:x :p 0 .""".stripMargin
    val shex =
      """|prefix : <http://e#>
           |
           |abstract :A { }
           |:B @:A AND { :p . }
           |""".stripMargin
    shouldValidateWithShapeMap(rdf, shex, ":x@:B", ":x@:B, :x@:A", Debug)
  }

}
