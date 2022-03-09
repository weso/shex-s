package es.weso.shex.validator.validatorref

import es.weso.shex.validator.ShapeTyping
import es.weso.shapemaps.FixedShapeMap

case class State(typing: ShapeTyping)

object State {
  def from(shapeMap: FixedShapeMap): State = State(ShapeTyping.emptyShapeTyping)
}
