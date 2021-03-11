package es.weso.utils.testsuite
import cats.effect._
import cats.implicits._

case class NotFoundTestEntry(id: TestId, entries: List[TestEntry]) extends RuntimeException
case class MoreThanOneTestEntry(ids: List[TestEntry], entries: List[TestEntry]) extends RuntimeException

case class TestSuite(tests: List[TestEntry]) {

  /**
    * Run a list of test entries 
    * @return A pair formed by the entries that passed and the entries that failed
    */
  def runAll(config: TestConfig): IO[Vector[TestResult]] = for {
    refStats <- Ref[IO].of(Stats(tests)) 
    _ <- tests.map(_.runEntry(refStats,config)).sequence.void
    endStats <- refStats.get
    _ <- if (config.verbose) IO.println(endStats.show) else IO.unit
  } yield (endStats.failed ++ endStats.passed)

  def runSingle(testId: TestId, config: TestConfig): IO[TestResult] = {
    tests.filter(_.name == testId) match {
      case Nil => IO.raiseError(NotFoundTestEntry(testId, tests))
      case entry :: Nil => for {
        refStats <- Ref[IO].of(Stats(List(entry)))
        result <- entry.runEntry(refStats,config)
      } yield result
      case xs => IO.raiseError(MoreThanOneTestEntry(xs, tests))
    }
  } 
    
}
