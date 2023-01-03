package es.weso.wshex.matcher
import munit._

class ErtaAleTest extends FunSuite {

  val ertaStr =
    """|{"type":"item","id":"Q903","labels":{"es":{"language":"es","value":"Erta Ale"},"en":{"language":"en","value":"Erta Ale"},"fr":{"language":"fr","value":"Erta Ale"},"it":{"language":"it","value":"Erta Ale"},"nb":{"language":"nb","value":"Erta Ale"},"nl":{"language":"nl","value":"Erta Ale"},"en-ca":{"language":"en-ca","value":"Erta Ale"},"en-gb":{"language":"en-gb","value":"Erta Ale"},"de":{"language":"de","value":"Erta Ale"},"he":{"language":"he","value":"\u05d0\u05e8\u05d8\u05d4 \u05d0\u05dc\u05d4"},"sv":{"language":"sv","value":"Erta Ale"},"sk":{"language":"sk","value":"Irta\u2019ale Isat\u00e4 G\u00e4mora"},"fi":{"language":"fi","value":"Erta Ale"},"hu":{"language":"hu","value":"Erta Ale"},"hr":{"language":"hr","value":"Erta Ale"},"ar":{"language":"ar","value":"\u0625\u0631\u062a\u0627 \u0623\u0644\u064a\u0647"},"cs":{"language":"cs","value":"Erta Ale"},"el":{"language":"el","value":"\u0388\u03c1\u03c4\u03b1 \u0386\u03bb\u03b5"},"hi":{"language":"hi","value":"\u0907\u0930\u094d\u091f\u093e \u090f\u0932\u0947"},"ja":{"language":"ja","value":"\u30a8\u30eb\u30bf\u30fb\u30a2\u30ec"},"ko":{"language":"ko","value":"\uc5d0\ud2b8\ub77c \uc5d0\uc77c \uc0b0"},"lt":{"language":"lt","value":"Erta Al\u0117"},"oc":{"language":"oc","value":"Erta Ale"},"pl":{"language":"pl","value":"Erta Ale"},"ro":{"language":"ro","value":"Erta Ale"},"ru":{"language":"ru","value":"\u042d\u0440\u0442\u0430 \u0410\u043b\u0435"},"sh":{"language":"sh","value":"Erta Ale"},"uk":{"language":"uk","value":"\u0415\u0440\u0442\u0430-\u0410\u043b\u0435"},"ta":{"language":"ta","value":"\u0b8e\u0bb0\u0bcd\u0b9f\u0bbe \u0b8f\u0bb2\u0bcd"},"zh":{"language":"zh","value":"\u5c14\u5854\u963f\u96f7\u706b\u5c71"},"ca":{"language":"ca","value":"Erta Ale"},"ne":{"language":"ne","value":"\u090f\u0930\u094d\u091f\u093e\u200c \u090f\u0932"},"az":{"language":"az","value":"Erta Ale"},"tr":{"language":"tr","value":"Erta Ale"},"zh-hant":{"language":"zh-hant","value":"\u723e\u5854\u963f\u96f7\u706b\u5c71"},"ceb":{"language":"ceb","value":"Irta'\u0101l\u0113 Isate Gemora YeFelek'ebet Gudgwad"},"da":{"language":"da","value":"Erta Ale"},"mk":{"language":"mk","value":"\u0415\u0440\u0442\u0430 \u0410\u043b\u0435"},"th":{"language":"th","value":"\u0e20\u0e39\u0e40\u0e02\u0e32\u0e44\u0e1f\u0e40\u0e2d\u0e40\u0e23\u0e2d\u0e15\u0e32\u0e40\u0e25"},"sr":{"language":"sr","value":"\u0415\u0440\u0442\u0430 \u0410\u043b\u0435"},"gl":{"language":"gl","value":"Erta Ale"},"fa":{"language":"fa","value":"\u0627\u0631\u062a\u0627 \u0622\u0644\u06cc"},"hy":{"language":"hy","value":"\u0537\u0580\u057f\u0561 \u0531\u056c\u0565"},"bg":{"language":"bg","value":"\u0415\u0440\u0442\u0430 \u0410\u043b\u0435"}},"descriptions":{"es":{"language":"es","value":"volc\u00e1n de Etiop\u00eda"},"en":{"language":"en","value":"volcano in Ethiopia"},"fr":{"language":"fr","value":"volcan d'\u00c9thiopie"},"it":{"language":"it","value":"catena di vulcani dell'Etiopia"},"nb":{"language":"nb","value":"vulkan i Etiopia"},"de":{"language":"de","value":"basaltischer Schildvulkan im Nordosten \u00c4thiopiens"},"ru":{"language":"ru","value":"\u0432\u0443\u043b\u043a\u0430\u043d \u0432 \u042d\u0444\u0438\u043e\u043f\u0438\u0438"},"zh-hans":{"language":"zh-hans","value":"\u4f4d\u4e8e\u57c3\u585e\u4fc4\u6bd4\u4e9a\u7684\u706b\u5c71"},"hu":{"language":"hu","value":"m\u0171k\u00f6d\u0151 t\u0171zh\u00e1ny\u00f3, Eti\u00f3pia"},"nl":{"language":"nl","value":"berg in Ethiopi\u00eb"},"da":{"language":"da","value":"vulkan i Etiopien"},"gl":{"language":"gl","value":"volc\u00e1n de Etiop\u00eda"},"pl":{"language":"pl","value":"wulkan w Etiopii"},"ar":{"language":"ar","value":"\u0628\u0631\u0643\u0627\u0646 \u062f\u0631\u0639\u064a \u0641\u064a \u0625\u062b\u064a\u0648\u0628\u064a\u0627"}},"aliases":{},"claims":{"P17":[{"mainsnak":{"snaktype":"value","property":"P17","datavalue":{"value":{"entity-type":"item","numeric-id":115,"id":"Q115"},"type":"wikibase-entityid"},"datatype":"wikibase-item"},"type":"statement","id":"Q903$9e48e71f-4a10-7f1e-d981-1a5b7487d5ab","rank":"normal"}],"P31":[{"mainsnak":{"snaktype":"value","property":"P31","datavalue":{"value":{"entity-type":"item","numeric-id":212057,"id":"Q212057"},"type":"wikibase-entityid"},"datatype":"wikibase-item"},"type":"statement","id":"Q903$DB49A482-8515-4368-93AF-BC188F451A2D","rank":"normal"}],"P373":[{"mainsnak":{"snaktype":"value","property":"P373","datavalue":{"value":"Erta Ale","type":"string"},"datatype":"string"},"type":"statement","id":"q903$1CA2C492-0567-43B0-B0A5-C68D3BE719A8","rank":"normal"}],"P625":[{"mainsnak":{"snaktype":"value","property":"P625","datavalue":{"value":{"latitude":13.603,"longitude":40.6634,"altitude":null,"precision":0.0001,"globe":"http:\/\/www.wikidata.org\/entity\/Q2"},"type":"globecoordinate"},"datatype":"globe-coordinate"},"type":"statement","id":"q903$E7E0CB91-4D5B-4F46-9320-98982022E3BD","rank":"preferred"}],"P18":[{"mainsnak":{"snaktype":"value","property":"P18","datavalue":{"value":"ET Afar asv2018-01 img98 way to Ertale.jpg","type":"string"},"datatype":"commonsMedia"},"type":"statement","id":"Q903$7581B597-BB5B-4455-B577-F1F7D3CD281C","rank":"normal"}],"P646":[{"mainsnak":{"snaktype":"value","property":"P646","datavalue":{"value":"\/m\/03trbr","type":"string"},"datatype":"external-id"},"type":"statement","id":"Q903$605179EE-6F9D-49D6-8EBC-332E22D3C514","rank":"normal","references":[{"hash":"2b00cb481cddcac7623114367489b5c194901c4a","snaks":{"P248":[{"snaktype":"value","property":"P248","datavalue":{"value":{"entity-type":"item","numeric-id":15241312,"id":"Q15241312"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}],"P577":[{"snaktype":"value","property":"P577","datavalue":{"value":{"time":"+2013-10-28T00:00:00Z","timezone":0,"before":0,"after":0,"precision":11,"calendarmodel":"http:\/\/www.wikidata.org\/entity\/Q1985727"},"type":"time"},"datatype":"time"}]},"snaks-order":["P248","P577"]}]}],"P1566":[{"mainsnak":{"snaktype":"value","property":"P1566","datavalue":{"value":"334073","type":"string"},"datatype":"external-id"},"type":"statement","id":"Q903$1DAC0FAD-D620-4AA4-A79F-8B73AB121064","rank":"normal","references":[{"hash":"88694a0f4d1486770c269f7db16a1982f74da69d","snaks":{"P248":[{"snaktype":"value","property":"P248","datavalue":{"value":{"entity-type":"item","numeric-id":830106,"id":"Q830106"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}]},"snaks-order":["P248"]}]}],"P1886":[{"mainsnak":{"snaktype":"value","property":"P1886","datavalue":{"value":"221080","type":"string"},"datatype":"external-id"},"type":"statement","id":"Q903$D162466F-7788-479A-8DD3-30EEB6DCF022","rank":"normal"}],"P2044":[{"mainsnak":{"snaktype":"value","property":"P2044","datavalue":{"value":{"amount":"+613","unit":"http:\/\/www.wikidata.org\/entity\/Q11573"},"type":"quantity"},"datatype":"quantity"},"type":"statement","id":"Q903$57422681-6670-413E-ADE2-4B0624FA8E63","rank":"normal","references":[{"hash":"9a24f7c0208b05d6be97077d855671d1dfdbc0dd","snaks":{"P143":[{"snaktype":"value","property":"P143","datavalue":{"value":{"entity-type":"item","numeric-id":48183,"id":"Q48183"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}]},"snaks-order":["P143"]}]}],"P186":[{"mainsnak":{"snaktype":"value","property":"P186","datavalue":{"value":{"entity-type":"item","numeric-id":43338,"id":"Q43338"},"type":"wikibase-entityid"},"datatype":"wikibase-item"},"type":"statement","id":"Q903$ECF41C9C-5F14-4255-9BFD-CBF941166D3C","rank":"normal","references":[{"hash":"d4bd87b862b12d99d26e86472d44f26858dee639","snaks":{"P143":[{"snaktype":"value","property":"P143","datavalue":{"value":{"entity-type":"item","numeric-id":8447,"id":"Q8447"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}]},"snaks-order":["P143"]}]}],"P361":[{"mainsnak":{"snaktype":"value","property":"P361","datavalue":{"value":{"entity-type":"item","numeric-id":81591,"id":"Q81591"},"type":"wikibase-entityid"},"datatype":"wikibase-item"},"type":"statement","id":"Q903$C64641C5-0B7B-461E-8F20-F56EF97B58D3","rank":"normal","references":[{"hash":"d4bd87b862b12d99d26e86472d44f26858dee639","snaks":{"P143":[{"snaktype":"value","property":"P143","datavalue":{"value":{"entity-type":"item","numeric-id":8447,"id":"Q8447"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}]},"snaks-order":["P143"]}]}],"P4552":[{"mainsnak":{"snaktype":"value","property":"P4552","datavalue":{"value":{"entity-type":"item","numeric-id":5395879,"id":"Q5395879"},"type":"wikibase-entityid"},"datatype":"wikibase-item"},"type":"statement","id":"Q903$468A88AB-1746-4EFA-8FDB-5F5B3D47EB90","rank":"normal","references":[{"hash":"fa278ebfc458360e5aed63d5058cca83c46134f1","snaks":{"P143":[{"snaktype":"value","property":"P143","datavalue":{"value":{"entity-type":"item","numeric-id":328,"id":"Q328"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}]},"snaks-order":["P143"]}]}],"P131":[{"mainsnak":{"snaktype":"value","property":"P131","datavalue":{"value":{"entity-type":"item","numeric-id":3444683,"id":"Q3444683"},"type":"wikibase-entityid"},"datatype":"wikibase-item"},"type":"statement","id":"Q903$6C01A703-FD57-41F4-9CD6-A992CFA0EDBA","rank":"normal","references":[{"hash":"d4bd87b862b12d99d26e86472d44f26858dee639","snaks":{"P143":[{"snaktype":"value","property":"P143","datavalue":{"value":{"entity-type":"item","numeric-id":8447,"id":"Q8447"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}]},"snaks-order":["P143"]}]}],"P2581":[{"mainsnak":{"snaktype":"value","property":"P2581","datavalue":{"value":"00440718n","type":"string"},"datatype":"external-id"},"type":"statement","id":"Q903$1AB73911-7116-4DA1-B60C-76985D5E3042","rank":"normal","references":[{"hash":"248ac337a217a5f5eed7139a82a4e60931611af0","snaks":{"P248":[{"snaktype":"value","property":"P248","datavalue":{"value":{"entity-type":"item","numeric-id":4837690,"id":"Q4837690"},"type":"wikibase-entityid"},"datatype":"wikibase-item"}]},"snaks-order":["P248"]}]}],"P3219":[{"mainsnak":{"snaktype":"value","property":"P3219","datavalue":{"value":"erta-ale","type":"string"},"datatype":"external-id"},"type":"statement","id":"Q903$B7DAE3A0-184B-4A1D-9E4A-94A9471B1915","rank":"normal"}],"P4708":[{"mainsnak":{"snaktype":"value","property":"P4708","datavalue":{"value":"108","type":"string"},"datatype":"external-id"},"type":"statement","id":"Q903$91b61e2b-4eff-4eb8-32a8-a016dc62be06","rank":"normal"}],"P948":[{"mainsnak":{"snaktype":"value","property":"P948","datavalue":{"value":"Wv Erta Ale banner.jpg","type":"string"},"datatype":"commonsMedia"},"type":"statement","id":"Q903$b4a18ace-40d5-88ac-254a-29da102910b8","rank":"normal"}],"P214":[{"mainsnak":{"snaktype":"value","property":"P214","datavalue":{"value":"315128323","type":"string"},"datatype":"external-id"},"type":"statement","id":"Q903$C1DF8D30-DB98-4DA0-9B71-4BB31DAB6D0D","rank":"normal"}],"P7772":[{"mainsnak":{"snaktype":"value","property":"P7772","datavalue":{"value":"erta-ale","type":"string"},"datatype":"external-id"},"type":"statement","id":"Q903$CD13E3AA-019A-4A77-9A52-4E2204E6C6A7","rank":"normal"}]},"sitelinks":{"enwiki":{"site":"enwiki","title":"Erta Ale","badges":[]},"dewiki":{"site":"dewiki","title":"Erta Ale","badges":[]},"frwiki":{"site":"frwiki","title":"Erta Ale","badges":[]},"eswiki":{"site":"eswiki","title":"Erta Ale","badges":[]},"itwiki":{"site":"itwiki","title":"Erta Ale","badges":[]},"jawiki":{"site":"jawiki","title":"\u30a8\u30eb\u30bf\u30fb\u30a2\u30ec","badges":[]},"nlwiki":{"site":"nlwiki","title":"Erta Ale","badges":[]},"plwiki":{"site":"plwiki","title":"Erta Ale","badges":[]},"zhwiki":{"site":"zhwiki","title":"\u5c14\u5854\u963f\u96f7\u706b\u5c71","badges":[]},"svwiki":{"site":"svwiki","title":"Erta Ale","badges":[]},"hewiki":{"site":"hewiki","title":"\u05d0\u05e8\u05d8\u05d4 \u05d0\u05dc\u05d4","badges":[]},"trwiki":{"site":"trwiki","title":"Erta Ale","badges":[]},"huwiki":{"site":"huwiki","title":"Erta Ale","badges":[]},"fiwiki":{"site":"fiwiki","title":"Erta Ale","badges":[]},"arwiki":{"site":"arwiki","title":"\u0625\u0631\u062a\u0627 \u0623\u0644\u064a\u0647","badges":[]},"nowiki":{"site":"nowiki","title":"Erta Ale","badges":[]},"ukwiki":{"site":"ukwiki","title":"\u0415\u0440\u0442\u0430-\u0410\u043b\u0435","badges":[]},"cswiki":{"site":"cswiki","title":"Erta Ale","badges":[]},"rowiki":{"site":"rowiki","title":"Erta Ale","badges":[]},"azwiki":{"site":"azwiki","title":"Erta Ale","badges":[]},"cebwiki":{"site":"cebwiki","title":"Irta'\u0101l\u0113 Isate Gemora YeFelek'ebet Gudgwad","badges":[]},"elwiki":{"site":"elwiki","title":"\u0388\u03c1\u03c4\u03b1 \u0386\u03bb\u03b5","badges":[]},"hiwiki":{"site":"hiwiki","title":"\u0907\u0930\u094d\u091f\u093e \u090f\u0932\u0947","badges":[]},"hrwiki":{"site":"hrwiki","title":"Erta Ale","badges":[]},"ltwiki":{"site":"ltwiki","title":"Erta Al\u0117","badges":[]},"ocwiki":{"site":"ocwiki","title":"Erta Ale","badges":[]},"shwiki":{"site":"shwiki","title":"Erta Ale","badges":[]},"skwiki":{"site":"skwiki","title":"Irta\u2019ale Isat\u00e4 G\u00e4mora","badges":[]},"ruwiki":{"site":"ruwiki","title":"\u042d\u0440\u0442\u0430\u043b\u0435","badges":[]},"mkwiki":{"site":"mkwiki","title":"\u0415\u0440\u0442\u0430 \u0410\u043b\u0435","badges":[]},"thwiki":{"site":"thwiki","title":"\u0e20\u0e39\u0e40\u0e02\u0e32\u0e44\u0e1f\u0e40\u0e2d\u0e40\u0e23\u0e2d\u0e15\u0e32\u0e40\u0e25","badges":[]},"kowiki":{"site":"kowiki","title":"\uc5d0\ub974\ud0c0 \uc54c\ub808 \uc0b0","badges":[]},"commonswiki":{"site":"commonswiki","title":"Category:Erta Ale","badges":[]},"itwikivoyage":{"site":"itwikivoyage","title":"Erta Ale","badges":[]},"srwiki":{"site":"srwiki","title":"\u0415\u0440\u0442\u0430 \u0410\u043b\u0435","badges":[]},"glwiki":{"site":"glwiki","title":"Erta Ale","badges":[]},"fawiki":{"site":"fawiki","title":"\u0627\u0631\u062a\u0627 \u0622\u0644\u06cc","badges":[]},"hywiki":{"site":"hywiki","title":"\u0537\u0580\u057f\u0561 \u0531\u056c\u0565","badges":[]},"simplewiki":{"site":"simplewiki","title":"Erta Ale","badges":[]},"dawiki":{"site":"dawiki","title":"Erta Ale","badges":[]},"cawiki":{"site":"cawiki","title":"Erta Ale","badges":[]},"bgwiki":{"site":"bgwiki","title":"\u0415\u0440\u0442\u0430 \u0410\u043b\u0435","badges":[]}},"lastrevid":1369073059},
               |""".stripMargin

  test("Q903_Volcano") {
    val schemaStr = """|prefix : <http://www.wikidata.org/entity/>
                       |
                       |start = @<Volcano>
                       |
                       |<Volcano> {
                       |  :P31 [ :Q212057 ]
                       |}""".stripMargin
    val eitherMatcher = Matcher.unsafeFromString(schemaStr)
    eitherMatcher.fold(
      parseError => fail(s"Error parsing schema: $parseError"),
      matcher => {
        val matchStatus = matcher.matchJsonStart(ertaStr)
        assert(matchStatus.matches, s"Doesn't match Q903\nMatchStatus=${matchStatus})")
      }
    )
  }

  test("Q903_Volcano part of") {
    val schemaStr = """|prefix : <http://www.wikidata.org/entity/>
                       |
                       |start = @<Volcano>
                       |
                       |<Volcano> {
                       |  :P31  [ :Q212057 ] ; # instance of Volcano
                       |  :P361 .              # part of 
                       |}""".stripMargin
    val eitherMatcher = Matcher.unsafeFromString(schemaStr)
    eitherMatcher.fold(
      parseError => fail(s"Error parsing schema: $parseError"),
      matcher => {
        val matchStatus = matcher.matchJsonStart(ertaStr)
        assert(matchStatus.matches, s"Doesn't match Q903\nMatchStatus=${matchStatus})")
      }
    )
  }
}
