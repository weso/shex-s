package es.weso.wshex.matcher
import munit._
import es.weso.wshex.WShExFormat

class ActiveSiteTest extends FunSuite {

  val str =
    """|{"type":"item","id":"Q24721060","labels":{"en":{"language":"en","value":"4-hydroxybenzoyl-CoA thioesterase, active site"}},"descriptions":{"en":{"language":"en","value":"InterPro Active Site"},"fr":{"language":"fr","value":"Site Actif InterPro"}},"aliases":{"en":[{"language":"en","value":"HB-CoA_thioesterase_AS"},{"language":"en","value":"IPR008272"}]},"claims":{"P2926":[{"mainsnak":{"snaktype":"value","property":"P2926","datavalue":{"value":"IPR008272","type":"string"},"datatype":"external-id"},"type":"statement","id":"Q24721060$A120894D-EEDD-4B7A-8E05-A12927E231F6","rank":"normal","references":[{"hash":"bb83fd6542e7e67b59e21449f939fc045a9ed793","snaks":{"P248":[{"snaktype":"value","property":"P248","datavalue":{"value":{"entity-type":"item","numeric-id":58943792,"id":"Q58943792"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}],"P2926":[{"snaktype":"value","property":"P2926","datavalue":{"value":"IPR008272","type":"string"},"datatype":"external-id"}]},"snaks-order":["P248","P2926"]}]}],"P361":[{"mainsnak":{"snaktype":"value","property":"P361","datavalue":{"value":{"entity-type":"item","numeric-id":24723599,"id":"Q24723599"},"type":"wikibase-entityid"},"datatype":"wikibase-item"},"type":"statement","id":"Q24721060$32855782-9948-46BF-A0DF-0BD92ED0DA4A","rank":"normal","references":[{"hash":"bb83fd6542e7e67b59e21449f939fc045a9ed793","snaks":{"P248":[{"snaktype":"value","property":"P248","datavalue":{"value":{"entity-type":"item","numeric-id":58943792,"id":"Q58943792"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}],"P2926":[{"snaktype":"value","property":"P2926","datavalue":{"value":"IPR008272","type":"string"},"datatype":"external-id"}]},"snaks-order":["P248","P2926"]}]},{"mainsnak":{"snaktype":"value","property":"P361","datavalue":{"value":{"entity-type":"item","numeric-id":24741232,"id":"Q24741232"},"type":"wikibase-entityid"},"datatype":"wikibase-item"},"type":"statement","id":"Q24721060$DEC59F8C-79BB-431D-BB02-2DAD4F9A42C1","rank":"normal","references":[{"hash":"bb83fd6542e7e67b59e21449f939fc045a9ed793","snaks":{"P248":[{"snaktype":"value","property":"P248","datavalue":{"value":{"entity-type":"item","numeric-id":58943792,"id":"Q58943792"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}],"P2926":[{"snaktype":"value","property":"P2926","datavalue":{"value":"IPR008272","type":"string"},"datatype":"external-id"}]},"snaks-order":["P248","P2926"]}]},{"mainsnak":{"snaktype":"value","property":"P361","datavalue":{"value":{"entity-type":"item","numeric-id":24722506,"id":"Q24722506"},"type":"wikibase-entityid"},"datatype":"wikibase-item"},"type":"statement","id":"Q24721060$01F759EE-D9D8-4EF2-90F5-4C78EAA6436E","rank":"normal","references":[{"hash":"bb83fd6542e7e67b59e21449f939fc045a9ed793","snaks":{"P248":[{"snaktype":"value","property":"P248","datavalue":{"value":{"entity-type":"item","numeric-id":58943792,"id":"Q58943792"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}],"P2926":[{"snaktype":"value","property":"P2926","datavalue":{"value":"IPR008272","type":"string"},"datatype":"external-id"}]},"snaks-order":["P248","P2926"]}]}],"P31":[{"mainsnak":{"snaktype":"value","property":"P31","datavalue":{"value":{"entity-type":"item","numeric-id":423026,"id":"Q423026"},"type":"wikibase-entityid"},"datatype":"wikibase-item"},"type":"statement","id":"Q24721060$A2CEAF71-0FDF-4F9F-8C7A-CB91C9D06E74","rank":"normal","references":[{"hash":"bb83fd6542e7e67b59e21449f939fc045a9ed793","snaks":{"P248":[{"snaktype":"value","property":"P248","datavalue":{"value":{"entity-type":"item","numeric-id":58943792,"id":"Q58943792"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}],"P2926":[{"snaktype":"value","property":"P2926","datavalue":{"value":"IPR008272","type":"string"},"datatype":"external-id"}]},"snaks-order":["P248","P2926"]}]}],"P680":[{"mainsnak":{"snaktype":"value","property":"P680","datavalue":{"value":{"entity-type":"item","numeric-id":14353168,"id":"Q14353168"},"type":"wikibase-entityid"},"datatype":"wikibase-item"},"type":"statement","qualifiers":{"P4390":[{"snaktype":"value","property":"P4390","hash":"cf2e72f259de6985120c9cce1364b7bc8c87f8d9","datavalue":{"value":{"entity-type":"item","numeric-id":39894595,"id":"Q39894595"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}]},"qualifiers-order":["P4390"],"id":"Q24721060$AA731DED-07E0-4766-AB2A-5912383760F2","rank":"normal","references":[{"hash":"1a30bd8e6f89c23aa3d1ed77bac8e64f5e6481e6","snaks":{"P248":[{"snaktype":"value","property":"P248","datavalue":{"value":{"entity-type":"item","numeric-id":102425430,"id":"Q102425430"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}],"P2926":[{"snaktype":"value","property":"P2926","datavalue":{"value":"IPR008272","type":"string"},"datatype":"external-id"}]},"snaks-order":["P248","P2926"]}]}]},"sitelinks":{},"pageid":26684265,"ns":0,"title":"Q24721060","lastrevid":1318327885,"modified":"2020-12-05T17:12:54Z"}""".stripMargin

  test("ActiveSite") {
    val schemaStr = """|PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                       |PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                       |PREFIX wd: <http://www.wikidata.org/entity/>
                       |PREFIX wdt: <http://www.wikidata.org/prop/direct/>
                       |PREFIX :    <http://example.org/>
                       |
                       |start= @:active_site OR 
                       |       @:anatomical_structure 
                       |
                       |:active_site EXTRA wdt:P31 {
                       |  rdfs:label [ @en @es ] * ;
                       |  wdt:P31  [ wd:Q423026 ]      ;
                       |  wdt:P361 @:protein_family * ;
                       |}
                       |
                       |:anatomical_structure EXTRA wdt:P31 {
                       |  rdfs:label [ @en @es ] * ;  
                       |  wdt:P31  [ wd:Q4936952 ] ;
                       |  wdt:P361 @:anatomical_structure * ; 
                       |  wdt:P527 @:anatomical_structure *
                       |}
                       |""".stripMargin
    val eitherMatcher = Matcher.unsafeFromString(str = schemaStr, format = WShExFormat.ESCompactFormat)
    eitherMatcher.fold(
      parseError => fail(s"Error parsing schema: $parseError"),
      matcher => {
        val matchStatus = matcher.matchJsonStart(str)
        assert(matchStatus.matches, s"Doesn't match Q903\nMatchStatus=${matchStatus})")
      }
    )
  }

}
