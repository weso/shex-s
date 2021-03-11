package es.weso.utils.testsuite
import cats._
import cats.implicits._
import scala.concurrent.duration.FiniteDuration

sealed abstract class TestResult extends Product with Serializable {
  val passed: Boolean
}
case class PassedResult(
  entry: TestId, 
  msg: Option[String] = None,
  time: Option[FiniteDuration] = None
 ) extends TestResult {
  override val passed = true
}

case class FailedResult(
  entry: TestId, 
  msg: Option[String] = None,
  time: Option[FiniteDuration] = None, 
  maybeException: Option[Throwable] = None
 ) extends TestResult {
 override val passed = false
}

object FailedResult {

  implicit val showResult: Show[FailedResult] = new Show[FailedResult] {
    def show(fr: FailedResult): String = s"Failed: ${fr.msg.fold("")(str => s"$str ")}${fr.entry.show} time: ${fr.time}.${fr.maybeException.fold("")(e => s"\nException: ${e.getMessage}")}"
  }  
}

