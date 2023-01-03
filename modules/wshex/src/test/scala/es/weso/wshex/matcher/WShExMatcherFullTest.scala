package es.weso.wshex.matcher
import munit._
import es.weso.wshex.WShExFormat._
import org.wikidata.wdtk.datamodel.helpers
import es.weso.utils.VerboseLevel
import org.wikidata.wdtk.datamodel.interfaces.{Reference => WDTKReference, _}
import org.wikidata.wdtk.datamodel.helpers._
import org.wikidata.wdtk.datamodel.helpers.ItemDocumentBuilder._
import org.wikidata.wdtk.datamodel.implementation._
import es.weso.wbmodel._
import cats.implicits._

/** Test matcher using WShEx as input */
class WShExMatcherFullTest extends CheckMatchWShEx {

  val dataObjectFactory = new DataObjectFactoryImpl();

  val defaultSite = "http://www.wikidata.org/entity/"
  val emptyDoc = forItemId(ItemIdValue.NULL)

  def Q(num: Long, site: String = defaultSite): ItemDocumentBuilder = {
    val id: ItemIdValue = new ItemIdValueImpl(s"Q$num", site)
    ItemDocumentBuilder.forItemId(id)
  }

  {
    val q42 = Q(42).build()

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> {
                       |}""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(Q(42).build()).some
    checkMatch(":Q42 == <S> {}", schemaStr, q42, expected)
  }

  {
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val p31_q5 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .build()
    val p19_q6 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P19", defaultSite))
        .withValue(Q(6).build().getEntityId())
        .build()
    val q42_p31_q5 = q42_raw.withStatement(p31_q5)
    val q42_full = q42_p31_q5.withStatement(p19_q6)

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<Human>
                       |
                       |<Human> {
                       |  :P31 [ :Q5 ]
                       |}""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_p31_q5).some
    checkMatch(":Q42 :P31 :Q5 . = <S> { :P31 [ :Q5 ] }", schemaStr, q42_full, expected)
  }

  {
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val douglas = new StringValueImpl("Douglas Adams")
    val p31_q5 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .build()
    val p734_adams =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P734", defaultSite))
        .withValue(new StringValueImpl("Adams"))
        .build()
    val q42_p734_adams = q42_raw.withStatement(p734_adams)
    val q42_full = q42_p734_adams.withStatement(p31_q5)

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> {
                       |  :P734 /Ad/ ;
                       |}
                       |""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_p734_adams).some
    checkMatch(
      ":Q42 :P31 :Q5; :P734 \"Douglas Adams\" . != <S> { :P734 /Ad/ }",
      schemaStr,
      q42_full,
      expected
    )
  }

  {
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val douglas = new StringValueImpl("Douglas Adams")
    val p31_q5 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .build()
    val p734_adams =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P734", defaultSite))
        .withValue(new StringValueImpl("Adams"))
        .build()
    val q42_p734_adams = q42_raw.withStatement(p734_adams)
    val q42_full = q42_p734_adams.withStatement(p31_q5)

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<Human>
                       |
                       |<Douglas> {
                       |  :P734 /Foo/ ;
                       |}""".stripMargin
    val expected: Option[EntityDoc] = None
    checkMatch(
      ":Q42 :P31 :Q5; :P734 \"Douglas Adams\" . != <S> { :P734 /Foo/ }",
      schemaStr,
      q42_full,
      expected
    )
  }

  {
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val q6 = Q(6).build()
    val p31_q5 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .build()
    val p31_q6 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q6.getEntityId())
        .build()
    val q42_full = q42_raw.withStatement(p31_q5).withStatement(p31_q6)
    val q42_p31q5 = q42_raw.withStatement(p31_q5)

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> EXTRA :P31 {
                       |  :P31 [ :Q5 ] ;
                       |}""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_p31q5).some
    checkMatch(
      ":Q42 :P31 :Q5, :Q6 . == <S> EXTRA :P31 { :P31 [ :Q5 ] }",
      schemaStr,
      q42_full,
      expected
    )
  }

  { // :Q42 :P31 :Q5, :Q6 . # <S> { :p31 [ :Q5 :Q6 :Q7 ] }
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val q6 = Q(6).build()
    val p31_q5 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .build()
    val p31_q6 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q6.getEntityId())
        .build()
    val q42_full = q42_raw.withStatement(p31_q5).withStatement(p31_q6)

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> {
                       |  :P31 [ :Q5 :Q6 :Q7 ] + ;
                       |}""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_full).some
    checkMatch(":Q42 :P31 :Q5, :Q6 . # <S> { :P31 [ :Q5 :Q6 :Q7 ] }", schemaStr, q42_full, expected)
  }

  {
    val msg = ":Q42 :P31 :Q5 . # <S> @<T> OR @<U> | <t> { :p31 [ :Q515 ] } | <U> { :P31 [ :Q5 ] }"
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val q6 = Q(6).build()
    val p31_q5 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .build()
    val q42_full = q42_raw.withStatement(p31_q5)

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S> OR @<T>
                       |
                       |<S> { :P31 [ :Q515] }
                       |<T> { :P31 [ :Q5 ] } 
                       |""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_full).some
    checkMatch(msg, schemaStr, q42_full, expected)
  }

  {
    val q42_raw = Q(42).build()
    val ten = dataObjectFactory.getQuantityValue(BigDecimal.valueOf(10.0).bigDecimal)
    val p1_ten =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P1", defaultSite))
        .withValue(ten)
        .build()
    val q42_p1_ten = q42_raw.withStatement(p1_ten)

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> {
                       |  :P1 MinInclusive 5 ;
                       |}""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_p1_ten).some
    checkMatch(":Q42 :P1 10 . == <S> { :P1 MinInclusive 5 }", schemaStr, q42_p1_ten, expected)
  }

  {
    val q42_raw = Q(42).build()
    val ten = dataObjectFactory.getQuantityValue(BigDecimal.valueOf(10.0).bigDecimal)
    val p1_ten =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P1", defaultSite))
        .withValue(ten)
        .build()
    val q42_p1_ten = q42_raw.withStatement(p1_ten)

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> {
                       |  :P1 MinInclusive 20 ;
                       |}""".stripMargin
    val expected: Option[EntityDoc] = None
    checkMatch(":Q42 :P1 10 . != <S> { :P1 MinInclusive 20 }", schemaStr, q42_p1_ten, expected)
  }

  {
    val q42_raw = Q(42).build()
    val ten = dataObjectFactory.getQuantityValue(BigDecimal.valueOf(10.0).bigDecimal)
    val p1_ten =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P1", defaultSite))
        .withValue(ten)
        .build()
    val q42_p1_ten = q42_raw.withStatement(p1_ten)

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> {
                       |  :P1 MinInclusive 20 ;
                       |}""".stripMargin
    val expected: Option[EntityDoc] = None
    checkMatch(":Q42 :P1 10 . != <S> { :P1 MinInclusive 20 }", schemaStr, q42_p1_ten, expected)
  }

  { // :Q42 :P31 :Q5, :Q6 . # <S> { :p31 [ :Q5 :Q6 :Q7 ] }
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val q6 = Q(6).build()
    val p31_q5 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .build()
    val p31_q6 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q6.getEntityId())
        .build()
    val q42_full = q42_raw.withStatement(p31_q5).withStatement(p31_q6)

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> {
                       |  :P31 [ :Q5 :Q6 :Q7 ] + ;
                       |}""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_full).some
    checkMatch(":Q42 :P31 :Q5, :Q6 . # <S> { :P31 [ :Q5 :Q6 :Q7 ] }", schemaStr, q42_full, expected)
  }

  {
    val label = ":Q42 :P31 :Q5 References {| :P248 :Q6 |} . # <S> { :p31 . {| :P248 . |} }"
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val q6 = Q(6).build()
    val ref1: WDTKReference = ReferenceBuilder
      .newInstance()
      .withPropertyValue(new PropertyIdValueImpl("P248", defaultSite), q6.getEntityId())
      .build()
    val p31_q5 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .withReference(ref1)
        .build()
    val q42_full = q42_raw.withStatement(p31_q5)

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> {
                       |  :P31 . References {| :P248 . |} ;
                       |}""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_full).some
    checkMatch(label, schemaStr, q42_full, expected)
  }

  {
    val label =
      ":Q42 :P31 :Q5 References {| :P248 :Q6 ; :P214 \"hi\" |} . # <S> { :p31 . {| :P248 . ; |} }"
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val q6 = Q(6).build()
    val hi = new StringValueImpl("Hi")
    val p31_q5_refs =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .withReference(
          ReferenceBuilder
            .newInstance()
            .withPropertyValue(new PropertyIdValueImpl("P248", defaultSite), q6.getEntityId())
            .withPropertyValue(new PropertyIdValueImpl("P2148", defaultSite), hi)
            .build()
        )
        .build()
    val q42_full = q42_raw.withStatement(p31_q5_refs)
    val p31_q5_refExpected =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .withReference(
          ReferenceBuilder
            .newInstance()
            .withPropertyValue(new PropertyIdValueImpl("P248", defaultSite), q6.getEntityId())
            .build()
        )
        .build()
    val q42_expected = q42_raw.withStatement(p31_q5_refExpected)
    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> {
                       |  :P31 . References {| :P248 . |} ;
                       |}""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_expected).some
    checkMatch(label, schemaStr, q42_full, expected)
  }

  {
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val q350 = Q(350).build()
    val p31_q5 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .build()
    val p19_q350 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P19", defaultSite))
        .withValue(q350.getEntityId())
        .build()
    val q42_all =
      q42_raw
        .withStatement(p31_q5)
        .withStatement(p19_q350)

    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> {
                       |  :P31 [ :Q5 ] ;
                       |  :P19 @<Place> 
                       |}
                       |
                       |<Place> Label (en -> .) {
                       |}
                       |""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_all).some
    checkMatch(
      ":Q42 :P31 q5 ; :P19 :Q350 . == <S> { :P31 [ :Q5 ] ; :P19 @<Place>}",
      schemaStr,
      q42_all,
      expected
    )
  }

  {
    val label =
      ":Q42 :P31 :Q5 References {| :P248 :Q6 ; :P214 \"hi\" |} . # <S> { :p31 . {| :P248 . ; :P214 . |} }"
    val q42_raw = Q(42).build()
    val q5 = Q(5).build()
    val q6 = Q(6).build()
    val hi = new StringValueImpl("Hi")
    val p31_q5_refs =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .withReference(
          ReferenceBuilder
            .newInstance()
            .withPropertyValue(new PropertyIdValueImpl("P248", defaultSite), q6.getEntityId())
            .withPropertyValue(new PropertyIdValueImpl("P214", defaultSite), hi)
            .build()
        )
        .build()
    val q42_full = q42_raw.withStatement(p31_q5_refs)
    val p31_q5_refExpected =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .withReference(
          ReferenceBuilder
            .newInstance()
            .withPropertyValue(new PropertyIdValueImpl("P214", defaultSite), hi)
            .withPropertyValue(new PropertyIdValueImpl("P248", defaultSite), q6.getEntityId())
            .build()
        )
        .build()
    val q42_expected = q42_raw.withStatement(p31_q5_refExpected)
    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> {
                       |  :P31 . References {| :P248 [ :Q6 ] ; :P214 . |} ;
                       |}""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_expected).some
    checkMatch(label, schemaStr, q42_full, expected)
  } 

  {
    val q42_raw = Q(42).withLabel("Douglas", "es").withLabel("Douglas", "en").build()
    val q5 = Q(5).build()
    val p31_q5 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .build()
    val q42 = q42_raw.withStatement(p31_q5)
    val q42_onlyEnglish = Q(42).withLabel("Douglas", "en").build().withStatement(p31_q5)
    
    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> Label (en -> .) {
                       |  :P31 [ :Q5 ] ;
                       |}
                       |""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_onlyEnglish).some
    
    checkMatch(
      ":Q42 Label \"Douglas\"; :P31 q5 . == <S> Label (en -> .) { :P31 [ :Q5 ] }",
      schemaStr,
      q42,
      expected,
      VerboseLevel.Nothing
    )
  }  

  {
    val q42_raw = Q(42).withDescription("Escritor", "es").withDescription("Writer", "en").build()
    val q5 = Q(5).build()
    val p31_q5 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .build()
    val q42 = q42_raw.withStatement(p31_q5)
    val q42_onlyEnglish = Q(42).withDescription("Writer", "en").build().withStatement(p31_q5)
    
    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> Description (en -> .) {
                       |  :P31 [ :Q5 ] ;
                       |}
                       |""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_onlyEnglish).some
    
    checkMatch(
      ":Q42 Description \"Writer\"; :P31 q5 . == <S> Description (en -> .) { :P31 [ :Q5 ] }",
      schemaStr,
      q42,
      expected,
      VerboseLevel.Nothing
    )
  }  


  {
    val q42_raw = Q(42).withAlias("Dug", "es").withAlias("Dou", "en").build()
    val q5 = Q(5).build()
    val p31_q5 =
      StatementBuilder
        .forSubjectAndProperty(q42_raw.getEntityId(), new PropertyIdValueImpl("P31", defaultSite))
        .withValue(q5.getEntityId())
        .build()
    val q42 = q42_raw.withStatement(p31_q5)
    val q42_onlyEnglish = Q(42).withAlias("Dou", "en").build().withStatement(p31_q5)
    
    val schemaStr = """|prefix :  <http://www.wikidata.org/entity/>
                       |
                       |start = @<S>
                       |
                       |<S> Alias (en -> .) {
                       |  :P31 [ :Q5 ] ;
                       |}
                       |""".stripMargin
    val expected: Option[EntityDoc] = EntityDoc(q42_onlyEnglish).some
    
    checkMatch(
      ":Q42 Alias \"Dou\"@en; :P31 q5 . == <S> Alias (en -> .) { :P31 [ :Q5 ] }",
      schemaStr,
      q42,
      expected,
      VerboseLevel.Nothing
    )
  }  
}
