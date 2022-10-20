package es.weso.wshex.matcher

import es.weso.wbmodel.{Lang => WDTKLang, _}
import org.wikidata.wdtk.datamodel.implementation._
import org.wikidata.wdtk.datamodel.interfaces._
import es.weso.rdf.nodes._
import es.weso.wshex._
import es.weso.wshex.TermConstraint.StringConstraint
import es.weso.wshex.TermConstraint.StringConstraintMatchError

sealed abstract class MatchingError(msg: String) extends Product with Serializable

object MatchingError {

  case class Pending(
      msg: String
  ) extends MatchingError(s"Pending: $msg")

  case class NotImplemented(msg: String) extends MatchingError(s"Not Implemented: $msg")

  case class NoShapeExprs(
      wShEx: WSchema
  ) extends MatchingError(s"No shape expressions in schema ${wShEx}")

  case class NoStatementGroupProperty(
      property: PropertyIdValue,
      entityDocument: EntityDocument
  ) extends MatchingError(s"No statement group for property $property\nEntity: ${entityDocument
          .getEntityId()}")

  case class NoStatementMatchesValue(
      predicate: IRI,
      value: IRI,
      entityDocument: EntityDocument
  ) extends MatchingError(s"""|No statements matches predicate ${predicate} with value ${value}
                                |Entity: $entityDocument""".stripMargin)

  case class NoStatementDocument(
      entityDocument: EntityDocument
  ) extends MatchingError(s"""|Entity is not an StatementDocument
                                |Entity: ${entityDocument}""".stripMargin)

  case class NotShapeFail(se: WShapeExpr, entity: EntityDoc)
      extends MatchingError(s"""|NOT failed because entity matches shapeExpr
                                              |Entity: ${entity.show()}
                                              |ShapeExpr: $se
                                              |""".stripMargin)

  case class NoValuesProperty(property: IRI, entity: EntityDoc)
      extends MatchingError(s"""|No values for property: ${property}
                                              |Entity ${entity.show()}
                                              |""".stripMargin)

  case class NoMatchTermConstraint(tc: TermConstraint, msg: String, entity: EntityDoc)
      extends MatchingError(s"""|No matching for term constraint: ${tc}
                              |Message: $msg
                              |Entity ${entity.show()}
                              |""".stripMargin)

  case class LabelConstraintNoLang(lang: Lang, entity: EntityDoc)
      extends MatchingError(s"""|No label with lang: ${lang}
                              |Entity ${entity.show()}
                              |""".stripMargin)

  case class DescriptionConstraintNoLang(lang: Lang, entity: EntityDoc)
      extends MatchingError(s"""|No description with lang: ${lang}
                              |Entity ${entity.show()}
                              |""".stripMargin)

  case class AliasConstraintNoLang(lang: Lang, entity: EntityDoc)
      extends MatchingError(s"""|No alias with lang: ${lang}
                              |Entity ${entity.show()}
                              |""".stripMargin)

  case class StringConstantMatchingError(s: String, expected: String)
      extends MatchingError(s"""|String matching error
                              |current : ${s}
                              |expected: ${expected}
                              |""".stripMargin)

  case class RegexMatchingError(value: String, pattern: String, flags: String, msg: String = "")
      extends MatchingError(s"""|Regex matching error
                              |value: ${value}
                              |pattern: ${pattern}
                              |flags: ${flags}
                              |msg: $msg
                              |""".stripMargin)

  case class StringConstraintError(err: StringConstraintMatchError, tc: StringConstraint, value: MonolingualTextValue)
      extends MatchingError(s"""|TermConstraint MatchError
                                |StringConstraint: $tc
                                |value: $value
                                |err: ${err}
                              |""".stripMargin)                              

}
