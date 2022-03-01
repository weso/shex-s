package es.weso.shex.validator
import es.weso.utils.VerboseLevel._

class ExtendsTestSingle extends ShouldValidateShapeMap {

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
//      shouldValidateWithShapeMap(rdf, shex, ":ok1@:A", ":ok1@:A")
      shouldValidateWithShapeMap(rdf, shex, ":ko1@:A", ":ko1@!:A", Debug)
    }

/*    {
      val rdf =
        """|prefix : <http:e/>
           |PREFIX foaf: <http://xmlns.com/foaf/>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |
           |:issue1
           |    :reportedBy     :bob .
           |
           |:bob foaf:givenName  "Bob" ;
           |     foaf:familyName "Smith" ;
           |     foaf:mbox       <mail:bob@example.org> ;
           |     :representative :joe
           |.
           |
           |:joe foaf:givenName  "Joe", "Joseph" ;
           |     foaf:familyName "Thompson" ;
           |     foaf:phone      <tel:+456> ;
           |     foaf:mbox       <mail:joe@example.org>
           |.
           |""".stripMargin
      val shex =
        """|prefix : <http:e/>
           |PREFIX foaf: <http://xmlns.com/foaf/>
           |PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
           |PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           |
           |:IssueShape CLOSED {
           |  :reportedBy @:PersonShape;
           |  :reproducedBy @:EmployeeShape ?
           |}
           |
           |ABSTRACT :PersonShape {
           |  (  foaf:name xsd:string |
           |     foaf:givenName xsd:string+;
           |     foaf:familyName xsd:string );
           |  foaf:mbox IRI
           |}
           |
           |:UserShape EXTENDS @:PersonShape CLOSED {
           |  :representative @:EmployeeShape
           |}
           |
           |ABSTRACT :RepShape {
           |  foaf:phone IRI+
           |}
           |
           |:EmployeeShape
           |    EXTENDS @:PersonShape
           |    EXTENDS @:RepShape CLOSED {
           |}
           |""".stripMargin
      shouldValidateWithShapeMap(rdf, shex, 
      ":issue1@:IssueShape", 
      ":issue1@:IssueShape", Debug)
    }
*/
} 
