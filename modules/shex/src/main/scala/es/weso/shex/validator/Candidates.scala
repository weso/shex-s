package es.weso.shex.validator
import es.weso.utils.SeqUtils._

case class Candidates(cs: List[Candidate]) extends AnyVal {

  def getCandidateLines(): List[CandidateLine] = {
    transpose(
      cs.map(c => (c.arc, c.crefs))
    ).map(ls => CandidateLine(ls.map { case (a, c) => ArcConstraintRef(a, c) }))
  }

}
