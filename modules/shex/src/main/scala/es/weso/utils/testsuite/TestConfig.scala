package es.weso.utils.testsuite
import cats.implicits._
import scala.concurrent.duration._

case class TestConfig(
  maxTimePerTest: FiniteDuration,
  verbose: Boolean
)

object TestConfig {
  def initial: TestConfig = TestConfig(5 second, true)
}
 