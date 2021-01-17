package es.weso.shapepath
import es.weso.shapepath._

sealed abstract class ProcessingError
case class UnmatchItemContextLabel(
  item: ShapeNode, 
  step: Step, 
  contextLabel: Context
  ) extends ProcessingError {
  override def toString = s"""|Processing error: 
                              |Item: ${item.toString}
                              |Step: ${step.toString}
                              |ContextLabel: ${contextLabel.toString}
                              |""".stripMargin
}

case class Warning(msg: String) extends ProcessingError {
  override def toString = s"Warning: $msg"
}

case class Err(msg: String) extends ProcessingError {
  override def toString = s"Error: $msg"
}
