package es.weso.utils.testsuite
import cats._
import cats.implicits._
import scala.concurrent.duration.FiniteDuration

sealed abstract class TestResult extends Product with Serializable {
  val passed: Boolean
} 

object TestResult {
 implicit val showResult: Show[TestResult] = new Show[TestResult] {
    def show(r: TestResult): String = r match {
      case fr: FailedResult => fr.show
      case pr: PassedResult => pr.show
    }
 }
}

case class PassedResult(
  entry: TestId, 
  msg: Option[String] = None,
  time: Option[FiniteDuration] = None
 ) extends TestResult {
  override val passed = true
} 

object PassedResult {
  implicit val showPassedResult: Show[PassedResult] = new Show[PassedResult] {
    def show(pr: PassedResult): String = s"Passed ${pr.entry.show}: ${pr.msg.fold("")(str => s"$str ")} time: ${pr.time}"
  }
}

case class FailedResult(
  entry: TestId, 
  msg: Option[String] = None,
  time: Option[FiniteDuration] = None, 
  exception: Option[Throwable] = None
 ) extends TestResult {
 override val passed = false
}

object FailedResult {
 implicit val showFailedResult: Show[FailedResult] = new Show[FailedResult] {
    def show(fr: FailedResult): String = s"Failed ${fr.entry.show}: ${fr.msg.fold("")(str => s"$str ")}${fr.entry.show} time: ${fr.time}.${fr.exception.fold("")(e => s"\nException: ${e.getMessage}")}"
 }
    
}

