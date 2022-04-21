package es.weso.shextest.manifest

sealed abstract class TestSelector {
  import TestSelector._
  def toOption: Option[String] = this match {
    case All        => None
    case Only(name) => Some(name)
  }

  def matches(other: String) = this match {
    case All                         => true
    case Only(name) if name == other => true
    case _                           => false
  }
}

object TestSelector {
  case object All extends TestSelector
  case class Only(name: String) extends TestSelector

  def fromOption(opt: Option[String]): TestSelector = opt match {
    case None    => All
    case Some(n) => Only(n)
  }
}
