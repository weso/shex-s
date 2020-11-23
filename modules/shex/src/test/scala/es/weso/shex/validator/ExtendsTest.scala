package es.weso.shex.validator

class ExtendsTest extends ShouldValidateShapeMap {

  describe("Simple Extends") {
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
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:A,:x@:B")
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
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:A,:x@:B")
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
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:A,:x@:B")
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
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:A,:x@:B")
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
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:B,:x@:A")
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
      shouldValidateWithShapeMap(rdf, shex, ":x@:A", ":x@:B,:x@:A")
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
      shouldValidateWithShapeMap(rdf, shex, ":x@:B", ":x@:B,:x@:A")
    }


  } // describe

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
} // class
