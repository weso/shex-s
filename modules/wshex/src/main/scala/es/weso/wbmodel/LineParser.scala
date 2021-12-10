package es.weso.wdsub.spark.wbmodel

import org.wikidata.wdtk.datamodel.helpers.JsonDeserializer
import org.apache.spark.graphx._
import DumpUtils._
import org.apache.spark.SparkContext

import java.nio.file.Path
import org.apache.spark.rdd.RDD
import org.apache.log4j.Logger
import es.weso.rdf.nodes.IRI
import org.apache.spark.storage.StorageLevel

case class LineParser(site: String = "http://www.wikidata.org/entity/") {

  lazy val jsonDeserializer = new JsonDeserializer(site)
  lazy val noEntity: Entity = Item(ItemId("Q0", IRI(site + "Q0")), 0L, Map(), Map(), Map(),site,List(),List())
  @transient lazy val log = Logger.getLogger(getClass.getName)

  def line2Entity(line: String): (Long,Entity) = try {
    val entityDocument = jsonDeserializer.deserializeEntityDocument(line)
    mkEntity(entityDocument)
  } catch {
    case e: Throwable => {
      log.error(s"""|line2Entity. exception parsing line: ${e.getMessage()}
                    |Line: ${line}
                    |""".stripMargin)
      (0L, noEntity)
    }
  }

  def line2Statement(line: String): List[Edge[Statement]] = try {
    val entityDocument = jsonDeserializer.deserializeEntityDocument(line)
    mkStatements(entityDocument)
  } catch {
    case e: Throwable => {
      log.error(s"""|line2Statement: Exception parsing line: ${e.getMessage()}
                    |Line: ${line}
                    |""".stripMargin)
      List()
    }
  }

  def lines2Entities(lines: List[String]): List[(Long,Entity)] =
    lines.map(line2Entity(_)).toList

  def lines2Statements(lines:List[String]): List[Edge[Statement]] =
    lines.map(line2Statement(_)).toList.flatten

  def line2EntityStatements(line: String): (Long,Entity,List[Edge[Statement]]) = try {
    val entityDocument = jsonDeserializer.deserializeEntityDocument(line)
    val (id, entity) = mkEntity(entityDocument)
    val ss = mkStatements(entityDocument)
    (id,entity,ss)
  } catch {
    case e: Throwable => {
      log.error(s"""|line2EntityStatements: exception parsing line: ${e.getMessage()}
                    |Line: ${line}
                    |""".stripMargin)
      (0L, noEntity, List())
    }
  }

  def dumpPath2Graph(path: Path, sc: SparkContext): Graph[Entity,Statement] = {
    val fileName = path.toFile().getAbsolutePath()
    val all: RDD[(Long,Entity, List[Edge[Statement]])] =
      sc
        .textFile(fileName)
        .filter(!brackets(_))
        .map(line2EntityStatements(_))

    val vertices: RDD[(Long,Entity)] =
      all
        .map { case (id,v,_) => (id,v) }

    val edges: RDD[Edge[Statement]] =
      all
        .map { case (_,_, ss) => ss }
        .flatMap(identity)

    Graph(vertices,edges)
  }

  def dumpRDD2Graph(dumpLines: RDD[String], sc: SparkContext): Graph[Entity,Statement] = {
    val all: RDD[(Long,Entity, List[Edge[Statement]])] =
      dumpLines
        .filter(!brackets(_))
        .map(line2EntityStatements(_))

    val vertices: RDD[(Long,Entity)] =
      all
        .map { case (id,v,_) => (id,v) }

    val edges: RDD[Edge[Statement]] =
      all
        .map { case (_,_, ss) => ss }
        .flatMap(identity)

    Graph(
      vertices = vertices,
      edges = edges,
      defaultVertexAttr = null,
      edgeStorageLevel = StorageLevel.DISK_ONLY_2,
      vertexStorageLevel = StorageLevel.DISK_ONLY_2
    )
  }

  def dump2Graph(dump:String, sc: SparkContext): Graph[Entity,Statement] = {
    val lines = dump.split("\n").filter(!brackets(_)).toList
    val vertices = sc.parallelize(lines2Entities(lines))
    val edges = sc.parallelize(lines2Statements(lines))
    Graph(vertices,edges)
  }
}