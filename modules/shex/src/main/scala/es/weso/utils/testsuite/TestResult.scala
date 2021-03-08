package es.weso.utils.testsuite
import cats._
import cats.implicits._
import scala.concurrent.duration.FiniteDuration

sealed abstract class TestResult extends Product with Serializable {
  val passed: Boolean
}
case class PassedResult(
  entry: TestEntry, 
  time: FiniteDuration
 ) extends TestResult {
  override val passed = true
}
case class FailedResult(
  entry: TestEntry, 
  time: FiniteDuration, 
  maybeException: Option[Throwable]
 ) extends TestResult {
 override val passed = false
}

object FailedResult {

  implicit val showResult: Show[FailedResult] = new Show[FailedResult] {
    def show(fr: FailedResult): String = s"Failed: ${fr.entry.show} time: ${fr.time}. ${fr.maybeException.fold("")(e => s"\nException: ${e.getMessage}")}"
  }  
}

