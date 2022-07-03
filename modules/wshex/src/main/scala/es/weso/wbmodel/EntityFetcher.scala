package es.weso.wbmodel

import org.wikidata.wdtk.wikibaseapi.WikibaseDataFetcher
import cats.effect._

object EntityFetcher {

  private lazy val wbdf: WikibaseDataFetcher = WikibaseDataFetcher.getWikidataDataFetcher()

  /** Get an entity from Wikidata
    *
    * @param entity
    * @return entity
    */
  def fetchEntity(entity: String): IO[EntityDocumentWrapper] = for {
    entityDocument <- IO(wbdf.getEntityDocument(entity))
  } yield EntityDocumentWrapper(entityDocument)

}
