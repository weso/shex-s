package es.weso.shex.validator


sealed abstract class ExtendsMode {
  def applyExtends: Boolean = this match {
    case ExtendsMode.ApplyExtends => true
    case _ => false
  }

  def show: String = this match {
    case ExtendsMode.ApplyExtends => "applyExtends"
    case ExtendsMode.IgnoreExtends => "ignoreExtends"
  }
}

object ExtendsMode {
  case object ApplyExtends extends ExtendsMode
  case object IgnoreExtends extends ExtendsMode
}
