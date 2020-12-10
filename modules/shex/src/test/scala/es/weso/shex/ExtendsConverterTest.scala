package es.weso.shex

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers


class ExtendsConverterTest extends AnyFunSpec with Matchers {
  describe(s"Extends converter Test") {
   /* shouldConvert(s"""|prefix fhir: <http://hl7.org/fhir/>
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
                      |ABSTRACT <#Vital> EXTENDS @<#Observation> {} # alias for Obs
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
                      |""".stripMargin, "") */
  /*    shouldConvert(s"""|prefix : <http://example.org/>
                      |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
                      |
                      |:Issue CLOSED {
                      | :reported   @:Person   ;
                      | :reproduced @:User     ;
                      | :related    @:Issue  *
                      |}
                      |
                      |:Person {
                      | :name xsd:string        | 
                      | :firstName xsd:string + ; 
                      | :lastName xsd:string 
                      |}
                      |
                      |:User @:Person AND {
                      |  :email IRI      ;
                      |  :knows @:User *
                      |}
                      |
                      |:SolvedIssue extends @:Issue {
                      | :reproduced   @:Employee   ;
                      | :solved       @:Programmer 
                      |}
                      |
                      |abstract :InternalRep {
                      | :code xsd:integer
                      |}
                      |
                      |:Employee extends @:Person 
                      |          extends @:InternalRep { }
                      |
                      |:Programmer extends @:Employee {
                      | :experience [ :senior :junior ]
                      |}
                      |""".stripMargin, 
                  s"""|prefix : <http://example.org/>
                      |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
                      |
                      |:Issue CLOSED {
                      | :reported   @:Person   ;
                      | :reproduced @:User     ;
                      | :related    @:Issue  *
                      |}  OR  {                   
                      | :reported @:Person;      
                      | :reproduced @:User;      
                      | :related @:Issue*;       
                      | :reproduced @:Employee;  
                      | :solved @:Programmer     
                      |} 
                      |
                      |:Person {
                      | :name xsd:string        | 
                      | :firstName xsd:string + ; 
                      | :lastName xsd:string 
                      |}
                      |
                      |:User @:Person AND {
                      |  :email IRI      ;
                      |  :knows @:User *
                      |}
                      |
                      |:SolvedIssue extends @:Issue {
                      | :reproduced   @:Employee   ;
                      | :solved       @:Programmer 
                      |}
                      |
                      |abstract :InternalRep {
                      | :code xsd:integer
                      |}
                      |
                      |:Employee extends @:Person 
                      |          extends @:InternalRep { }
                      |
                      |:Programmer extends @:Employee {
                      | :experience [ :senior :junior ]
                      |}
                      |""".stripMargin) */

shouldConvert(s"""|prefix : <http://example.org/>
                      |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
                      |
                      |<S1> {<p1> [1 2] } AND { <p1> [ 2 3 ]}
                      |<S2> EXTENDS @<S1> {<p2>.}
                      |<S3> EXTENDS @<S1> {<p3>.}
                      |ABSTRACT <S4> EXTENDS @<S2> EXTENDS @<S3> {<p4>.}
                      |<S5> EXTENDS @<S4> { <p5> @<S1> }
                      |
                      |""".stripMargin, 
                  s"""|
                      |""".stripMargin)                      
                           
    /* shouldConvert(s"""|prefix : <http://example.org/>
                      |prefix xsd: <http://www.w3.org/2001/XMLSchema#>
                      |
                      |:I { <c> . }
                      |:P { <p> . }
                      |:E extends @:P extends @:I {}
                      |:R extends @:E { <r> . }
                      |""".stripMargin, ""
                  )  */
                        

    def shouldConvert(schemaStr: String, expectedStr: String): Unit = {
     it(s"Should convert:\n${schemaStr}\n...and obtain:\n${expectedStr}") {
      val r = for {
        schema <- Schema.fromString(schemaStr,"SHEXC",None,None)
        resolvedSchema <- ResolvedSchema.resolve(schema,None)
        converted <- ExtendsConverter.convert(resolvedSchema)
      } yield (resolvedSchema, converted)
      r.attempt.unsafeRunSync().fold(e => {
         e.printStackTrace 
         fail(s"Error: ${e.getMessage}")
        }, 
        pair => { 
          val (resolved,converted) = pair
          println(s"""|Input:
                   |${resolved}
                   |-----------------------
                   |Converted:
                   |${converted}
                   |""".stripMargin)
          info(s"Conversion done")         
        })
     }
    }
  }
 
}
