package es.weso.wshex.es2wshex

sealed abstract class IRIParsed
case class DirectProperty(value: Int) extends IRIParsed
case class Property(value: Int) extends IRIParsed
case class PropertyStatement(value: Int) extends IRIParsed
case class PropertyQualifier(value: Int) extends IRIParsed
