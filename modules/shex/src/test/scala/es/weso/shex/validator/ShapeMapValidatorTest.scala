package es.weso.shex.validator

// import es.weso.rdf.jena.RDFAsJenaModel
// import cats.data._
// import cats.effect._
// import es.weso.utils.IOUtils._
// import es.weso.shex._
// import es.weso.rdf.triples._
// import es.weso.rdf.nodes._

class ShapeMapValidatorTest extends ShouldValidateShapeMap {

  { // shape Map
    val shexStr =
      """
        |prefix : <http://example.org/>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |
        |:S { :p . }
        |:CanVote xsd:integer MinInclusive 18
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p :b .
         |:c :p 1 .""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S", ":a@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S", ":a@:S,:b@!:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S,:c@:S", ":a@:S,:b@!:S,:c@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:a@:T", ":a@:S,:a@!:T")
    shouldValidateWithShapeMap(rdfStr, shexStr, "23@:CanVote", "23@:CanVote")
  }

  { // Recursive shape
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S { :p @:S }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p :b .
         |:b :p :a .
         |:c :p :c .
         |:d :p 1 .""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S", ":a@:S,:b@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S,:c@:S", ":a@:S,:b@:S,:c@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S,:c@:S,:d@:S", ":a@:S,:b@:S,:c@:S,:d@!:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":d@:S", ":d@!:S")
  }

  { // Two recursive shapes
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:S { :p @:T }
        |:T { :q @:S }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p :b .
         |:b :q :a .
         |:c :p :c .
         |:d :p 1 .""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S", ":a@:S,:b@:T")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":b@:T", ":a@:S,:b@:T")
  }

  // TODO: The following test fails...does the spec allow \d in regexes?
  { // Regular expressions
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { :p /\\d{2}/ }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p "23" .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A", ":a@:A")
  } 

  { // Shape with EXTRA
    val shexStr =
      """
        |prefix : <http://example.org/>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |
        |:S EXTRA :p { :p [ 1 ] }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p 1, 2 .
         |:b :p 1 .
         |:bad :p 2 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S", ":a@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S", ":a@:S,:b@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":b@:S", ":b@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S,:bad@:S", ":a@:S,:b@:S,:bad@!:S")
  }

   { // Shape with EXTRA and CLOSED
    val shexStr =
      """
        |prefix : <http://example.org/>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |
        |:S CLOSED EXTRA :p {
        | :p [ 1 2 3];
        | :p [ 3 4 5]
        |}
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p 1, 3 .
         |:b :p 2, 5, 7 .
         |:bad1 :p 2 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S,:b@:S,:bad1@:S", ":a@:S,:b@:S,:bad1@!:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S", ":a@:S")
  }

  { // Shape with EXTRA and CLOSED
    val shexStr =
      """
        |prefix : <http://example.org/>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |
        |:S CLOSED EXTRA :p {
        | :p [ 1 2 ];
        | :p [ 2 3 ]
        |}
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p 1, 2 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:S", ":a@:S")
  }

  { // Shape with inverse arcs
    val shexStr =
      """
        |prefix : <http://example.org/>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |
        |:S { ^:p @:T* }
        |:T { :q . }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:t1 :p :s1; :q "a" .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":s1@:S", ":s1@:S,:t1@:T")
  }

  {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { :p [@es] }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p "Hola"@es .
         |:x :p "Hi"@en .
         |:y :p "Hi" .
         |:z :p 23 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A", ":a@:A")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A,:x@:A,:y@:A,:z@:A", ":a@:A,:x@!:A,:y@!:A,:z@!:A")
  }

  {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { :p  [ <http://example.org/> ~ ] }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p <http://example.org/hi> .
         |:b :p :x .
         |:x :p <http://other.org/hi> .
         |:y :p "Hi" .
         |:z :p 23 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A", ":a@:A")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A,:b@:A", ":a@:A,:b@:A")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A,:b@:A,:x@:A,:y@:A,:z@:A", ":a@:A,:b@:A,:x@!:A,:y@!:A,:z@!:A")
  }
  
  {
    val shexStr =
      """
        |prefix : <http://example.org/>
		|PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        |:A { :p  @:List }
		|:List CLOSED { 
		|  rdf:first @:B ;
		|  rdf:rest [rdf:nil] OR @:List
		|}
		|:B { :q . }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
     |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
     |:a :p (:b1 :b2) .
		 |:b1 :q 1 .
     |:b2 :q 2 .
     |:ls rdf:first :b1; rdf:rest rdf:nil.
     |:x :p (:b1 :c1) .
     |:c1 :r 1 .
     |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":b1@:B", ":b1@:B")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":ls@:List", ":ls@:List,:b1@:B")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A,:ls@:List", ":a@:A,:ls@:List,:b1@:B,:b2@:B")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A,:x@:A,:ls@:List", ":a@:A,:ls@:List,:b1@:B,:b2@:B,:x@!:A")
  }
  
  {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |:A { $<lbl> (:p .; :q .) }
        |:B { :r . ; &<lbl> }
        |""".stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p 1 ; :q 1 .
     		 |:b :p 1 ; :q 1; :r 1 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A", ":a@:A")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@:A,:b@:B", ":a@:A,:b@:B")
  }

  {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |base <http://base.org/>
        |<A> { :p . }
        |""".stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:a :p 1 .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":a@<A>", ":a@<http://base.org/A>")
  }

  {
    val shexStr =
      """
        |PREFIX :       <http://example.org/>
        |PREFIX schema: <http://schema.org/>
        |PREFIX xsd:  <http://www.w3.org/2001/XMLSchema#>
        |
        |:User {
        |  schema:name          xsd:string  ;
        |  schema:birthDate     xsd:date?  ;
        |  schema:gender        [ schema:Male schema:Female ] OR xsd:string ;
        |  schema:knows         IRI @:User* ;
        |}
        |""".stripMargin
    val rdfStr =
      """|prefix schema: <http://schema.org/> 
         |prefix :       <http://example.org/>
         |
         |:alice schema:name "Alice" ; 
         |       schema:gender schema:Male .
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":alice@:User", ":alice@:User")
  }

  {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |
        |:S EXTRA :p { 
        |  :p [ 1 2 ] ;
        |  :q . +
        |}
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:ok1 :p 1 ; :q 1, 2 .
         |:ok2 :p 1, "hi"; :q "Hi" .
         |:ok3 :p 1, 3 ; :q 1, 2 .
         |:ko1 :p 1, 2 ; :q 1, 2 .
         |:ko2 :p 1, 2, "hi".
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":ok1@:S", ":ok1@:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":ok1@:S,:ok2@:S,:ok3@:S,:ko1@:S,:ko2@:S", ":ok1@:S,:ok2@:S,:ok3@:S,:ko1@!:S,:ko2@!:S")
  }

  {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |
        |:S { 
        | :p @:T ;
        | :p xsd:integer ;
        | } 
        |:T { :q . }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:ok1 :p :x, 1 . :x :q 1 .
         |:ko1 :p 1 . 
         |:ko2 :p 1, 2, 3 . 
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":ok1@:S", ":ok1@:S,:x@:T")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":ko1@:S", ":ko1@!:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":ko2@:S", ":ko2@!:S")
  } 

  {
    val shexStr =
      """
        |prefix : <http://example.org/>
        |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
        |
        |:S { 
        | :p @:T OR xsd:integer;
        | } 
        |:T { :q . }
      """.stripMargin
    val rdfStr =
      """|prefix : <http://example.org/>
         |:ok1 :p :x . :x :q 1 .
         |:ok2 :p 1 . 
         |:ko1 :p 1, 2 . 
         |:ko2 :p 1, 2, 3 . 
         |""".stripMargin

    shouldValidateWithShapeMap(rdfStr, shexStr, ":ok1@:S", ":ok1@:S,:x@:T")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":ok2@:S", ":ok2@:S",true)
    shouldValidateWithShapeMap(rdfStr, shexStr, ":ko1@:S", ":ko1@!:S")
    shouldValidateWithShapeMap(rdfStr, shexStr, ":ko2@:S", ":ko2@!:S")
  } 
}
