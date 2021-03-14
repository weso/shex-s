package es.weso.utils.testsuite
import cats.effect._
import cats.implicits._
import cats.effect.std.CountDownLatch

case class NotFoundTestEntry(id: TestId, entries: List[TestEntry]) extends RuntimeException
case class MoreThanOneTestEntry(ids: List[TestEntry], entries: List[TestEntry]) extends RuntimeException

case class TestSuite(tests: List[TestEntry]) {

  /**
    * Run the list of test entries 
    *
    * @param config configuration of running
    * @param except list of test ids that will be skipped
    * @return results of tests
    */  
  def runAll(config: TestConfig, except: List[TestId] = List()): IO[TestResults] = {
   
   // Tests that will be effectively run  
   val effectiveTests = tests.filter(t => ! (except contains t.id))

   // Tests that will be skipped
   val skipped = tests.map(_.id).filter(id => except contains id)

   // Tests in the except list not found in the existing tests
   val notFound = except.filter(e => !(tests.map(_.id) contains e))


   for {
    refStats <- Ref[IO].of(Stats(effectiveTests))

    // The latch is trying to enforce that the main fiber will wait untill all tests finnish
    // Is it necessary?
    latch <- CountDownLatch[IO](effectiveTests.length)

    fiber <- effectiveTests.map(test => for {
      r <- test.runEntry(refStats,config)
      _ <- latch.release
     } yield r).parSequence.start
    _ <- latch.await 
    
    endStats <- refStats.get
    _ <- if (config.verbose) 
      IO.println(endStats.show) else IO.unit
   } yield TestResults(endStats.passed, endStats.failed, skipped, notFound)

  }

  def runSingle(testId: TestId, config: TestConfig): IO[TestResult] = {
    tests.filter(_.id == testId) match {
      case Nil => IO.raiseError(NotFoundTestEntry(testId, tests))
      case entry :: Nil => for {
        refStats <- Ref[IO].of(Stats(List(entry)))
        result <- entry.runEntry(refStats,config)
      } yield result
      case xs => IO.raiseError(MoreThanOneTestEntry(xs, tests))
    }
  } 
    
}
