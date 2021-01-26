package es.weso.shapepath
import cats.Show
import cats.syntax.show._
import es.weso.rdf.PrefixMap
import java.awt.Label
import es.weso.rdf.nodes.IRI
import es.weso.shex.IRILabel

sealed abstract class Axis 
case object Child extends Axis
case object Descendant extends Axis
case object NestedShapeExpr extends Axis
case object NestedTripleExpr extends Axis
