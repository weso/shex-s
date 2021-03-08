package es.weso.utils.testsuite
import munit._
import cats.effect._
import cats.implicits._
import scala.concurrent.duration._

class TestSuiteTest extends CatsEffectSuite {

  def readTests(): IO[TestSuite] = 
    IO(TestSuite(List(mkEntry(1), mkEntry(2))))
  
  def mkAction(name: String, n: Int): IO[Boolean] = 
    IO(Integer.parseInt(name)).flatMap(v => IO.sleep(n.second) *> IO(v == n))

  def mkEntry(n: Int): TestEntry = TestEntry(TestId("E" + n), mkAction(n.toString, n))

  test("Run all tests") {
    val cmp = for {
      testSuite <- readTests()
      pair <- testSuite.runAll(TestConfig.initial)
    } yield (testSuite.tests.size, pair._1)
    cmp.map { case (total, passed) => assertEquals(passed.size, total) }
 }

 test("Run a single test") {
   val cmp = for {
     testSuite <- readTests()
     result <- testSuite.runSingle(TestId("E1"),TestConfig.initial) 
   } yield result
   cmp.map { case r => assertEquals(r.passed, true)}
 }

}