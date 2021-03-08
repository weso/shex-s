package es.weso.utils.testsuite
import cats._
import cats.implicits._
import scala.concurrent.duration.FiniteDuration
import scala.collection.immutable.HashSet

case class Stats private (pending: HashSet[TestEntry], passed: Vector[PassedResult], failed: Vector[FailedResult]) {
 
 def addResult(entry: TestEntry, result: TestResult): Stats = result match {
   case fr : FailedResult => this.copy(pending = this.pending - entry, failed = this.failed :+ fr)
   case pr : PassedResult => this.copy(pending - entry, passed = this.passed :+ pr)
 }  
 
}

object Stats {
    
  def apply(pending: List[TestEntry]): Stats = Stats(pending = HashSet(pending: _*), Vector(), Vector())

  implicit val showStats: Show[Stats] = new Show[Stats] {
        def show(s: Stats): String = 
          s"passed: ${s.passed.size}\nfailed: ${s.failed.size}\n${s.failed.map(_.show).mkString("\n")}"
  }
}

case class TimeOut(name: String, duration: FiniteDuration) extends RuntimeException(s"Timeout for test: $name. Timeout: $duration")
