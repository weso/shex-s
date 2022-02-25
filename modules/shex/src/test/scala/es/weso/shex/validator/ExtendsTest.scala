package es.weso.shex.validator
import es.weso.utils.VerboseLevel._

class ExtendsTest extends ShouldValidateShapeMap {

  {
      val rdf =
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
      val rdf =
        """|prefix : <http://e#>
           |:x :p "a", "b" .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:B { :p [ "b"] } AND { :p [ 1 ] }  # impossible
           |:A extends @:B {
           | :p [ "a" ]
           |}""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@!:A")
  }

  {
      val rdf =
        """|prefix : <http://e#>
           |:x :p 1 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |abstract :A { :p [1 2] }
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@!:A")
  } 

  {
      val rdf =
        """|prefix : <http://e#>
           |:x :p 1 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |abstract :A { :p [1 2] }
           |:B extends @:A {}
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:A, :x@:B")
  } 

  {
      val rdf =
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
      val rdf =
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
      val rdf =
        """|prefix : <http://e#>
           |:x :p 1, 2 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:B { :p [ 1 ] } 
           |:A extends @:B {
           | :p [ 2 ]
           |}""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:B", ":x@:B,:x@:A")
  }
  {
      val rdf =
        """|prefix : <http://e#>
           |:x :p 2 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |ABSTRACT :B { } 
           |:A extends @:B {} AND {
           | :p [ 2 ]
           |}""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:A")
    } 

    {
      val rdf =
        """|prefix : <http://e#>
           |:x :p 2 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:B { } 
           |:A extends @:B {}
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:A")
    } 

    {
      val rdf =
        """|prefix : <http://e#>
           |:x :p 2 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:A { :p [1] }
           |:B extends @:A { }
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@!:A")
    }
  
    {
      val rdf =
        """|prefix : <http://e#>
           |:x :p 0 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:A1 { :p . }
           |:A2 extends @:A1 { :p . }
           |:A3 extends @:A2 { :p . }
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A1", ":x@:A1")
    }
  
    {
      val rdf =
        """|prefix : <http://e#>
           |:x :p 0 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:A1 { :p [ 1 ] }
           |:A2 extends @:A1 { :p . }
           |:A3 extends @:A2 { :p . }
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:A1", ":x@!:A1")
    }
  
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
      shouldValidateWithShapeMap(rdf, shex, ":x@:R", ":x@!:R")
    } 

 /* { /* This test is different from Eric's implementation
         We assume that a node n conforms to an abstract shape S if there is a subshape T such that T extends @S and n conforms to T
         In this example, Eric assumes that :B extends :A
         */

      val rdf =
        """|prefix : <http://e#>
           |:x :p 0 .""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |
           |abstract :A { }
           |:B @:A AND { :p . }
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":x@:B", ":x@!:B")
  } */
  
/*
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
      shouldValidateWithShapeMap(rdf, shex, ":ok1@:A", ":ok1@:A,:ok1@:B")
      shouldValidateWithShapeMap(rdf, shex, ":ko1@:A", ":ko1@!:A")
      shouldValidateWithShapeMap(rdf, shex, ":ko2@:A", ":ko2@!:A")
  }  
  
  {
      val rdf =
        """|prefix : <http://e#>
           |:ok1 :p 1 .
           |:ko1 :p 1; :q 3 .
           |""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:B closed  { :p [ 1 ] }
           |:A extends @:B CLOSED { }
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":ok1@:A", ":ok1@:A,:ok1@:B")
      shouldValidateWithShapeMap(rdf, shex, ":ko1@:A", ":ko1@!:A")
  } */
/*    {
      /* This behaviour is different in Eric's implementation...
         If try with the different partitions.
          The partition { <p 1>,<q 3> matches @:B and { } matches CLOSED { } */

      val rdf =
        """|prefix : <http://e#>
           |:ok1 :p 1 .
           |:ko1 :p 1; :q 3 .
           |""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:B { :p [ 1 ] }
           |:A extends @:B CLOSED { }
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":ok1@:A", ":ok1@:A,:ok1@:B")
      // shouldValidateWithShapeMap(rdf, shex, ":ko1@:A", ":ko1@!:A")
      shouldValidateWithShapeMap(rdf, shex, ":ko1@:A", ":ko1@:A,:ko1@:B")
    } */


   

  /* {
      val rdf =
        """|prefix : <http://e#>
           |:ok1 :p 1; :q 2 .
           |""".stripMargin
      val shex =
        """|prefix : <http://e#>
           |:B { :p [ 1 ] }
           |:A extends @:B CLOSED {
           |}""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":ok1@:A", ":ok1@:A,:ok1@:B")
    } */


/*    describe("Open partition") {
   
    {
      val rdf =
        """|prefix foaf: <http://xmlns.com/foaf/0.1/>
           |prefix : <http://example.org/>
           |
           |:alice :code 8, 2, 3, 6 .
           |
           |""".stripMargin
      val shex =
                """|prefix foaf: <http://xmlns.com/foaf/0.1/>
                   |prefix : <http://example.org/>
                   |
                   |:InternalRep {
                   | :code [ 8 9 ] 
                   |}
                   |
                   |:User EXTENDS @:InternalRep {
                   |  :code [ 1 2 3 ] 
                   |}
                   |
                   |:Employee EXTENDS @:InternalRep {
                   |  :code [ 3 4 5 ]
                   |}
                   |
                   |# contrived example, sorry!
                   |:Alice extends @:User extends @:Employee AND {
                   |  :code [ 6 ]
                   |}
                   |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, ":alice@:Alice",":alice@:Alice")
    }

  } */


  /*

  describe(s"Users example") {
    val rdf="""|prefix : <http://e/>
               |
               |:alice :name "Alice" ;
               |       :rep :bob .
               |
               |:bob :name "Robert" ;
               |     :code "123" .
               |
               |:carol :name "Carol" ;
               |       :rep :dave .
               |:dave :code "234" ;
               |      :p    :other .
               |""".stripMargin
    val shex="""|prefix : <http://e/>
                |
                |abstract :Person { :name . }
                |:User extends @:Person closed { :rep @:Employee } 
                |
                |abstract :Rep { :code . }
                |:Employee extends @:Person extends @:Rep closed { }
                |""".stripMargin
    shouldValidateWithShapeMap(rdf, shex, ":alice@:User", ":alice@:User, :alice@:Person, :bob@:Employee, :bob@:Rep, :bob@:Person") 
    shouldValidateWithShapeMap(rdf, shex, ":dave@:Employee", ":dave@!:Employee") 
  }

  describe("People") {
    val shex = """|PREFIX ex: <http://ex.example/#>
                  |PREFIX foaf: <http://xmlns.com/foaf/>
                  |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
                  |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                  |
                  |<http://a.example/IssueShape> CLOSED {
                  |  ex:reportedBy @<http://a.example/PersonShape>;
                  |  ex:reproducedBy @<http://a.example/EmployeeShape>?
                  |}
                  |
                  |ABSTRACT <http://a.example/PersonShape> {
                  |  (  foaf:name xsd:string |
                  |     foaf:givenName xsd:string+;
                  |     foaf:familyName xsd:string);
                  |  foaf:mbox IRI
                  |}
                  |
                  |<http://a.example/UserShape>
                  |    EXTENDS @<http://a.example/PersonShape> CLOSED {
                  |  ex:representative @<http://a.example/EmployeeShape>
                  |}
                  |
                  |ABSTRACT <http://a.example/RepShape> {
                  |  foaf:phone IRI+
                  |}
                  |
                  |<http://a.example/EmployeeShape>
                  |    EXTENDS @<http://a.example/PersonShape>
                  |    EXTENDS @<http://a.example/RepShape> CLOSED {
                  |}
                  |""".stripMargin

    val rdf = """|PREFIX ex: <http://ex.example/#>
                 |PREFIX foaf: <http://xmlns.com/foaf/>
                 |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
                 |
                 |<http://a.example/Issue1>
                 |    ex:reportedBy   <http://a.example/User2> ;
                 |    ex:reproducedBy <http://a.example/Thompson.J> ;
                 |.
                 |
                 |<http://a.example/User2>
                 |    foaf:givenName "Bob" ;
                 |    foaf:familyName "Smith" ;
                 |    foaf:mbox <mail:bob@example.org> ;
                 |    ex:representative <http://a.example/Thompson.J>
                 |.
                 |
                 |<http://a.example/Thompson.J>
                 |    foaf:givenName "Joe", "Joseph" ;
                 |    foaf:familyName "Thompson" ;
                 |    foaf:phone <tel:+456> ;
                 |    foaf:mbox <mail:joe@example.org> ;
                 |    ex:p 1 .
                 |""".stripMargin

    shouldValidateWithShapeMap(rdf, shex, "<http://a.example/Thompson.J>@<http://a.example/EmployeeShape>", "<http://a.example/Thompson.J>@!<http://a.example/EmployeeShape>") 

  }
  */
/*
  describe(s"Vitals example") {
    val rdf = """|PREFIX : <http://a.example/#>
                 |PREFIX fhir: <http://hl7.org/ns/fhir#>
                 |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
                 |
                 |:lie fhir:component :lieA, :lieB, :lieC .
                 |:lieA fhir:code "systolic"; fhir:value 110 .
                 |:lieB fhir:code "diastolic"; fhir:value 70 .
                 |:lieC fhir:code "posture"; fhir:value "reclined" .
                 |
                 |:sit fhir:component :sitA, :sitB, :sitC .
                 |:sitA fhir:code "systolic"; fhir:value 110 .
                 |:sitB fhir:code "diastolic"; fhir:value 70 .
                 |:sitC fhir:code "posture"; fhir:value "sitting" .
                 |""".stripMargin
      val shex = """|BASE <http://a.example/>
                    |
                    |PREFIX : <http://hl7.org/ns/fhir>
                    |PREFIX fhir: <http://hl7.org/ns/fhir#>
                    |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
                    |
                    |<#Observation> {
                    |  fhir:code .? ;
                    |  fhir:component {
                    |    fhir:code . ;
                    |    fhir:value .
                    |  }*
                    |}
                    |
                    |# -- super-classes --
                    |ABSTRACT <#Vital> @<#Observation> AND .
                    |ABSTRACT <#PostureVital> @<#Vital> AND EXTENDS @<#Posture> {}
                    |ABSTRACT <#ReclinedVital> @<#PostureVital> AND EXTENDS @<#Reclined> {}
                    |
                    |# -- BP --
                    |<#BP> EXTENDS @<#Vital> {} AND {
                    |  fhir:component { fhir:code ["systolic"] } ;
                    |  fhir:component { fhir:code ["diastolic"] }
                    |}
                    |
                    |<#PostureBP> EXTENDS @<#BP> EXTENDS @<#PostureVital> { }
                    |<#ReclinedBP> EXTENDS @<#BP> EXTENDS @<#ReclinedVital> { }
                    |
                    |# -- Pulse --
                    |<#Pulse> EXTENDS @<#Vital> {} AND {
                    |    fhir:code ["pulse"]
                    |}
                    |<#PosturePulse> EXTENDS @<#Pulse> EXTENDS @<#PostureVital> { }
                    |<#ReclinedPulse> EXTENDS @<#Pulse> EXTENDS @<#ReclinedVital> { }
                    |
                    |# -- postures --
                    |<#Posture> {
                    |  fhir:component {
                    |    fhir:code ["posture"]
                    |  }
                    |}
                    |
                    |<#Reclined> @<#Posture> AND {
                    |  fhir:component {
                    |    fhir:code ["posture"] ;
                    |    fhir:value ["reclined"]
                    |  }
                    |}
                    |""".stripMargin
   shouldValidateWithShapeMap(rdf, shex, ":lie@<#Reclined>",
     """|:lie@<#Reclined>,
        |:lie@<#Posture>,
        |:lie@<#PostureBP>,
        |:lie@<#Posture>,
        |:lie@<#PostureVital>,
        |:lie@<#ReclinedBP>,
        |:lie@<#Observation>,
        |:lie@<#Vital>,
        |:lie@<#BP>,
        |:lie@<#ReclinedVital>
        |""".stripMargin)
    shouldValidateWithShapeMap(rdf, shex, ":sit@<#Reclined>",
       """|:sit@!<#Reclined>""".stripMargin
   )

 }  // describe 
 */
 
} // class
