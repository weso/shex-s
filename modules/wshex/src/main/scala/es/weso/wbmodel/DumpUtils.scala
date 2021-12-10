package es.weso.wdsub.spark.wbmodel

import es.weso.rdf.nodes.IRI
import org.apache.spark.graphx.Edge
import org.wikidata.wdtk.datamodel.interfaces.{EntityDocument, StatementDocument, Statement => WDStatement, Value => WDValue, _}

import scala.jdk.CollectionConverters._

object DumpUtils {

  lazy val PropertyIdDisplacement: Long = 10000000000L // We will assign vertex id's for items starting from 0 and for properties starting with this value
  lazy val StringIdDisplacement: Long = 20000000000L // We will assign vertex id's for items starting from 0 and for properties starting with this value


  def mkVertexId(value: WDValue): Long = {
    val item = """Q(\d+)""".r
    val prop = """P(\d+)""".r
    value match {
      case id: ItemIdValue => id.getId() match {
        case item(id) => id.toLong
      }
      case pd: PropertyIdValue => pd.getId() match {
        case prop(id) => id.toLong + PropertyIdDisplacement
      }
      case _ => 0L
    }
  }

  def brackets(line: String): Boolean = line.replaceAll("\\s", "") match {
    case "[" => true
    case "]" => true
    case _ => false
  }

  def mkEntity(ed: EntityDocument): (Long, Entity) = {
    val vertexId = mkVertexId(ed.getEntityId()) 
    ed match {
     case id: ItemDocument => { 
      val label = Option(id.findLabel("en")).getOrElse("")
      (vertexId, 
       Item(ItemId(id.getEntityId().getId(), IRI(id.getEntityId().getIri())), vertexId, Map(Lang("en") -> label), Map(), Map(), id.getEntityId().getSiteIri(), List(), List())
      ) 
      }
     case pd: PropertyDocument => {
      val label = Option(pd.findLabel("en")).getOrElse("")
      (vertexId, 
       Property(PropertyId(pd.getEntityId().getId(), IRI(pd.getEntityId().getIri())), vertexId, Map(Lang("en") -> label), Map(), Map(), pd.getEntityId().getSiteIri(), List())
      )
     }
    }
  }

  def mkStatement(s: WDStatement): Option[Edge[Statement]] = 
   s.getValue() match {
    case null => None
    case ev: EntityIdValue => {
      val subjectId = mkVertexId(s.getSubject())     
      val wdpid = s.getMainSnak().getPropertyId()
      val pid = PropertyId(wdpid.getId(), IRI(wdpid.getIri()))
      val pVertex = mkVertexId(wdpid)
      val valueId = mkVertexId(ev)
      // TODO. Collect qualifiers
      Some(Edge(subjectId, valueId, Statement(PropertyRecord(pid,pVertex))))
    }
    case _ => None
   }

  def mkStatements(ed: EntityDocument): List[Edge[Statement]] = {
    ed match {
     case sd: StatementDocument => { 
       sd.getAllStatements().asScala.toList.map(mkStatement).flatten
     }
     case _ => List[Edge[Statement]]()
    }
  }  

}

