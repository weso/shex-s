package es.weso.shex.validator
import es.weso.utils.VerboseLevel._

class ExtendsTestSingle extends ShouldValidateShapeMap {

  /*  {
    val rdf =
      """|PREFIX : <http://e/>
         |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
         |:lie :code "bp", "posture" .
         |""".stripMargin
    val shex =
      """|PREFIX : <http://e/>
         |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
         |
         |:Observation {
         |  :code . * ;
         |}
         |
         |ABSTRACT :Vital EXTENDS @:Observation { }
         |ABSTRACT :PostureVital EXTENDS @:Vital EXTENDS @:Posture { }
         |ABSTRACT :ReclinedVital EXTENDS @:PostureVital EXTENDS @:Reclined { }
         |
         |:BP EXTENDS @:Vital CLOSED { } AND {
         |  :code ["bp"]
         |}
         |
         |:PostureBP EXTENDS @:BP EXTENDS @:PostureVital  { }
         |:ReclinedBP EXTENDS @:BP EXTENDS @:ReclinedVital { }
         |
         |
         |:Posture {
         |  :code ["posture"]
         |}
         |
         |:Reclined EXTENDS @:Posture { }
         |
         |""".stripMargin

    // shouldValidateWithShapeMap(rdf, shex, ":lie@:PostureVital", ":lie@:PostureVital, :lie@:PostureBP, :lie@:ReclinedBP", Nothing)

    // shouldValidateWithShapeMap(rdf, shex, ":lie@:Posture", ":lie@:Posture, :lie@:ReclinedBP, :lie@:PostureBP", Nothing)

    shouldValidateWithShapeMap(rdf, shex, ":lie@:Reclined", ":lie@:Reclined", Debug)
  } */

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
//    shouldValidateWithShapeMap(rdf, shex, ":ok1@:A", ":ok1@:A")
    shouldValidateWithShapeMap(rdf, shex, ":ko1@:A", ":ko1@!:A", Debug)
  }
}
