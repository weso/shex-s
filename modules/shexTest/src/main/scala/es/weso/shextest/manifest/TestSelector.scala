package es.weso.shextest.manifest

sealed abstract class TestSelector {
  def matches(other: String): Boolean
}

object TestSelector {
  case object All extends TestSelector {
    override def matches(other: String) = true
  }

  case class Only(names: String*) extends TestSelector {
    override def matches(other: String) = names.toList.contains(other)
  }

  def fromOption(opt: Option[String]): TestSelector = opt match {
    case None    => All
    case Some(n) => Only(n)
  }
}
