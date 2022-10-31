package es.weso.shex.validator
import es.weso.utils.VerboseLevel._

class ExtendsTestSingle extends ShouldValidateShapeMap {

  {
    val rdf =
      """|prefix : <http://e#>
         |:x :p :y .
         |:y :f :v1 .
         |:y :r :z  .
         |:y :c "c" .
         |:z :f :v2 .
         |:z :r :n .""".stripMargin
    val shex =
      """|prefix : <http://e#>
         |:S CLOSED { :p @:T }
         |:T [ :n ] OR CLOSED { :f . ; :r @:T }
         |""".stripMargin
//    shouldValidateWithShapeMap(rdf, shex, ":y@:T", ":y@!:T", Debug)

    shouldValidateWithShapeMap(rdf, shex, ":x@:S", ":x@!:S", Debug)
  }


}
