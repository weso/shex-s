package es.weso.shexs

import com.monovore.decline.Opts
import cats.data.Validated._
import es.weso.shex.ShapeLabel

sealed abstract class SchemaAction {
  val name: String
}
object SchemaAction {

  case object ShowInheritanceGraph extends SchemaAction {
    val name = "inheritance"
  }

  case class ShowShape(shapeLabel: ShapeLabel) extends SchemaAction {
    val name = "shape"
  }

  val availableActions: List[SchemaAction] = List(ShowInheritanceGraph)
  val availableActionsStrs                 = availableActions.map(_.name)
  val availableActionsStr                  = availableActionsStrs.mkString(",")

  val schemaAction: Opts[SchemaAction] = Opts
    .option[String]("action", short = "a", help = s"action to run. Available actions =($availableActionsStr)")
    .mapValidated(s =>
      availableActions.collect { case a if a.name.toLowerCase() == s => a }.headOption match {
        case None    => invalidNel(s"Unknown value: $s")
        case Some(a) => valid(a)
      }
    )
}
