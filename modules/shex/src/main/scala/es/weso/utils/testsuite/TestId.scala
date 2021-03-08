package es.weso.utils.testsuite
import cats._

case class TestId(id: String) extends AnyVal

object TestId {
  implicit val showEntry: Show[TestId] = new Show[TestId] {
    def show(e: TestId): String = e.id
 }
}
