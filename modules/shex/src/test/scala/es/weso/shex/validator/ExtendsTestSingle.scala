package es.weso.shex.validator
import es.weso.utils.VerboseLevel._

class ExtendsTestSingle extends ShouldValidateShapeMap {

    {
      val rdf =
        """|prefix : <http://e#>
           |:x :p 2 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:P { }
           |:R { :p [1] }
           |:RV extends @:R {}
           |:RP extends @:RV {}
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:R", ":x@!:R", Debug)
    } 

} 
