package es.weso.shex.validator

class ExtendsTest extends ShouldValidateShapeMap {

  describe("Simple Extends") {

/*      {
        val rdf  =
          """|prefix : <http://e#>
             |:x :p 1, 3 .""".stripMargin
        val shex =
          """|prefix : <http://e#>
             |:B { :p [ 1 ] }
             |:A extends @:B {
             | :p [ 3 ]
             |}""".stripMargin
        shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:A")
      } 

    {
      val rdf  =
        """|prefix : <http://e#>
           |:x :p "a", "b" .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:B { :p [ "b"] } AND { :p [ 1 ] }  # impossible
           |:A extends @:B {
           | :p [ "a" ]
           |}""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@!:A")
    } */
 /*
     {
      val rdf  =
        """|prefix : <http://e#>
           |:x :p 1 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |abstract :A { :p [1 2] }
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@!:A")
    } */

    {
      val rdf  =
        """|prefix : <http://e#>
           |:x :p 1 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |abstract :A { :p [1 2] }
           |:B extends @:A {}
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:A")
    }

/*    {
      val rdf  =
        """|prefix : <http://e#>
           |:x :p 2, 3 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:B { :p [1 2] } AND { :p [2 3] }
           |:A extends @:B {
           | :p [2 3]
           |}""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:A")
    } 
    {
      val rdf  =
        """|prefix : <http://e#>
           |:x :p 1, 2 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:B { :p [ 1 ] } 
           |:A extends @:B {
           | :p [ 2 ]
           |}""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:A")
    } 
    {
      val rdf  =
        """|prefix : <http://e#>
           |:x :p 1, 2 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:B { :p [ 1 ] } 
           |:A extends @:B {
           | :p [ 2 ]
           |}""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:B", ":x@:B,:x@:A")
    } */

 }  // it
} // describe
