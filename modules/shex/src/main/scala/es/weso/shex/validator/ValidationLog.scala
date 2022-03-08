package es.weso.shex.validator

// import es.weso.rdf._
import cats._
import cats.implicits._
import es.weso.rdf.triples.RDFTriple
// import es.weso.utils.internal.CollectionCompat.LazyList


case class ValidationLog(actions: List[Action], slurp: List[RDFTriple])

object ValidationLog {

 implicit val validationLogMonoid: Monoid[ValidationLog] = new Monoid[ValidationLog] {
   def combine(l1: ValidationLog, l2: ValidationLog): ValidationLog = 
     ValidationLog(actions = l1.actions ++ l2.actions, slurp = l1.slurp ++ l2.slurp)
   def empty: ValidationLog = ValidationLog(actions = List(), slurp = List())
 }

 implicit val showValidationLog: Show[ValidationLog] = new Show[ValidationLog] {
   def show(x: ValidationLog): String = s"""|Actions: 
                                            |${x.actions.map(_.show).mkString("\n")}
                                            |Triples
                                            |${x.slurp.map(_.toString).mkString("\n")}
                                            |""".stripMargin
 }

}
