package es.weso.utils.testsuite
import cats._
import cats.implicits._
import cats.effect._

case class TestEntry(name: TestId, action: IO[Boolean]) {
 
 def runEntry(r: Ref[IO,Stats], config: TestConfig): IO[TestResult] = for {
    _ <- if (config.verbose) IO.println(s"Running ${name.show}") else IO.pure(())
    pair <- Clock[IO].timed(
      GenTemporal[IO].timeoutTo(
        action, config.maxTimePerTest, IO.raiseError(TimeOut(name.id,config.maxTimePerTest))
       ).attempt)
    (time, either) = pair  
    result <- either.fold(
        e => {
          val res = FailedResult(this, time, Some(e))
          r.getAndUpdate(_.addResult(this,res)) *> 
          IO.pure(res)
        }, 
        b => { 
         val res = if (b) PassedResult(this,time) 
           else FailedResult(this,time,None)
         r.getAndUpdate(_.addResult(this,res)) *> 
         IO.pure(res)
        }
    ) 
 } yield result

}

object TestEntry {
 implicit val showEntry: Show[TestEntry] = new Show[TestEntry] {
   def show(e: TestEntry): String = s"name: ${e.name.show}" 
 }
}