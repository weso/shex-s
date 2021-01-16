package es.weso.shapepath
import es.weso.shapepath.{Context, Item, Step}

sealed abstract class ProcessingError
case class UnmatchItemContextLabel(item: Item, step: Step, contextLabel: Context) extends ProcessingError {
  override def toString = s"Processing error: \n: Item: ${item.toString}\nStep: ${step.toString}\nContextLabel: ${contextLabel.toString}"
}

case class Warning(msg: String) extends ProcessingError {
  override def toString = s"Warning: $msg"
}

case class Err(msg: String) extends ProcessingError {
  override def toString = s"Error: $msg"
}
