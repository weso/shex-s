package es.weso.utils.testsuite

case class TestResults(
    passed: Vector[PassedResult],
    failed: Vector[FailedResult]
) {
   lazy val results: Vector[TestResult] = passed ++ failed
   lazy val allPassed: Boolean = failed.isEmpty
}