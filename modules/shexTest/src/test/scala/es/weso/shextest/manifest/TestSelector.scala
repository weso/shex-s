package es.weso.shextest.manifest

sealed abstract class TestSelector 

object TestSelector {
    case object All extends TestSelector
    case class Only(names: List[String]) extends TestSelector
}