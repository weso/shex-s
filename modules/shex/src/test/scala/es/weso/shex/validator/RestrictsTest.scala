package es.weso.shex.validator

class RestrictsTest extends ShouldValidateShapeMap {

  describe("Simple restricts") {
    {
      val rdf =
        """|prefix : <http://e#>
           |:ok1 :p 2, 3 .
           |:ko1 :p 1, 3 .
           |""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:B { :p [ 1 2 3 ]* }
           |:A restricts @:B {
           | :p [ 2 3 ]+
           |}""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":ok1@:A", ":ok1@:A,:ok1@:B")
      shouldValidateWithShapeMap(rdf, shex, ":ko1@:A", ":ko1@!:A")
      shouldValidateWithShapeMap(rdf, shex, ":ko1@:B", ":ko1@:B")
    }

    {
      val rdf =
        """|prefix : <http://e#>
           |:ok1 :p 2, 3 .
           |:ko1 :p 1, 3 .
           |""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |abstract :B { :p [ 1 2 3 ]* }
           |:A restricts @:B {
           | :p [ 2 3 ]+
           |}""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":ok1@:A", ":ok1@:A,:ok1@:B")
      shouldValidateWithShapeMap(rdf, shex, ":ok1@:B", ":ok1@:A,:ok1@:B")
      shouldValidateWithShapeMap(rdf, shex, ":ko1@:B", ":ko1@!:B")
    }
  } 
} // class
