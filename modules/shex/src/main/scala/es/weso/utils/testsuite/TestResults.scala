package es.weso.utils.testsuite

/**
  * Represents the results of running a list of tests
  *
  * @param passed tests that passed
  * @param failed tests that failed
  * @param skipped tests that were skipped
  * @param notFound tests in the except list that were not found in the list of tests to run
  */
case class TestResults(
    passed: Vector[PassedResult],
    failed: Vector[FailedResult],
    skipped: List[TestId],
    notFound: List[TestId] 
) {
   lazy val results: Vector[TestResult] = passed ++ failed
   lazy val allPassed: Boolean = failed.isEmpty
}