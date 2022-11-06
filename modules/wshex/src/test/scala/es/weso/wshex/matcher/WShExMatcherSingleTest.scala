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

/** Test matcher using Entity Schemas as input
  */
class WShExMatcherSingleTest extends CheckMatchWShEx {

  val dataObjectFactory = new DataObjectFactoryImpl();

  val defaultSite = "http://www.wikidata.org/entity/"
  val emptyDoc = forItemId(ItemIdValue.NULL)

  def Q(num: Long, site: String = defaultSite): ItemDocumentBuilder = {
    val id: ItemIdValue = new ItemIdValueImpl(s"Q$num", site)
    ItemDocumentBuilder.forItemId(id)
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

}
