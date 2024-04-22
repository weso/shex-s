package es.weso.wshex.matcher

import es.weso.wbmodel._
import org.wikidata.wdtk.datamodel.implementation._
import org.wikidata.wdtk.datamodel.interfaces.{
  Reference => WDTKReference,
  Snak => WDTKSnak,
  Statement => WDTKStatement,
  Value => WDTKValue,
  _
}
import es.weso.rdf.nodes._
import es.weso.wshex._
import es.weso.rbe.interval.IntOrUnbounded
import es.weso.utils.internal.CollectionCompat._
import es.weso.wshex.ReferencesSpec._
import es.weso.wshex.PropertySpec._
import es.weso.wshex.PropertySpec.PropertyConstraint._

sealed abstract class MatchingError(msg: String) extends Product with Serializable

object MatchingError {

  case class Pending(
      msg: String
  ) extends MatchingError(s"Pending: $msg")

  case class NotImplemented(msg: String) extends MatchingError(s"Not Implemented: $msg")

  case class InternalError(msg: String) extends MatchingError(s"Internal Error: $msg")

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

  /*  case class NoValuesProperty(property: IRI, entity: EntityDoc)
      extends MatchingError(s"""|No values for property: ${property}
                                              |Entity ${entity.show()}
                                              |""".stripMargin) */

  case class ValuesPropertyFailMin(property: IRI, entity: EntityDoc, counter: Int, min: Int)
      extends MatchingError(s"""|Values for property: ${property} = $counter should be > $min
                                |Entity ${entity.show()}
                                |""".stripMargin)

  case class ValuesPropertyFailMax(
      property: IRI,
      entity: EntityDoc,
      counter: Int,
      max: IntOrUnbounded
  ) extends MatchingError(s"""|Values for property: ${property} = $counter should be < $max
                                |Entity ${entity.show()}
                                |""".stripMargin)

  case class StatementsPropertyFailMin(
      property: IRI,
      counter: Int,
      min: Int,
      tcl: TripleConstraintLocal,
      entity: EntityDoc,
      oks: List[MatchingStatus],
      errs: List[MatchingStatus]
  ) extends MatchingError(s"""|Statements for property: ${property} = $counter should be > $min
                        |tripleConstraint: $tcl
                        |oks: $oks
                        |errs: $errs
                        |Entity ${entity.show()}
                        |""".stripMargin)

  case class StatementsPropertyFailMax(
      property: IRI,
      entity: EntityDoc,
      counter: Int,
      max: IntOrUnbounded
  ) extends MatchingError(s"""|Statements for property: ${property} = $counter should be < $max
                                |Entity ${entity.show()}
                                |""".stripMargin)

  case class StatementsPropertyRefFailMin(
      property: IRI,
      counter: Int,
      min: Int,
      tcr: TripleConstraintRef,
      entity: EntityDoc
  ) extends MatchingError(s"""|Statements for property: ${property} = $counter should be > $min
                        |tripleConstraintRef: $tcr
                        |Entity ${entity.show()}
                        |""".stripMargin)

  case class StatementsPropertyRefFailMax(
      property: IRI,
      entity: EntityDoc,
      counter: Int,
      max: IntOrUnbounded
  ) extends MatchingError(s"""|Statements for propertyRef: ${property} = $counter should be < $max
                                |Entity ${entity.show()}
                                |""".stripMargin)

  case class StatementsFailTripleConstraint(
      property: IRI,
      tcl: TripleConstraint,
      errs: List[MatchingStatus]
  ) extends MatchingError(s"""|Statements fail for property ${property} and tripleConstraint: ${tcl}
                              |Errors: $errs
                              |""".stripMargin)

  case class ValuesPropertyFailNodeConstraint(
      property: PropertyIdValue,
      wnc: WNodeConstraint,
      noMatched: LazyList[MatchingStatus]
  ) extends MatchingError(
        s"""|Some values for property: ${property} don't match nodeConstraint: $wnc. 
                                |Maybe you want to add EXTRA
                                |no matched values: ${noMatched.toList
             .map(_.toString)
             .mkString("\n")}
                                |""".stripMargin
      )

  case class ValuesPropertyFailNodeConstraintMin(
      property: PropertyIdValue,
      matchedCount: Int,
      min: Int,
      wnc: WNodeConstraint,
      noMatched: LazyList[MatchingStatus],
      matched: LazyList[MatchingStatus]
  ) extends MatchingError(s"""|#values that match node constraint = $matchedCount < $min
                       |${noMatched.length} values that fail to match: ${noMatched.toList.map(
                               _.toString.mkString("\n")
                             )}
                       |${matched.length} values that match: ${matched.toList
                               .map(_.toString)
                               .mkString("\n")}
                       |""".stripMargin)

  case class ValuesPropertyFailNodeConstraintMax(
      property: PropertyIdValue,
      matchedCount: Int,
      max: IntOrUnbounded,
      wnc: WNodeConstraint,
      matched: LazyList[MatchingStatus]
  ) extends MatchingError(s"""|#values that match node constraint = $matchedCount > $max
                                |Values that match: ${matched.toList.map(_.toString).mkString("\n")}
                                |""".stripMargin)

  case class NoMatchTermConstraint(tc: TermConstraint, msg: String, entity: EntityDoc)
      extends MatchingError(s"""|No matching for term constraint: ${tc}
                              |Message: $msg
                              |Entity ${entity.show()}
                              |""".stripMargin)

  case class LabelAnyNoLabel(entity: EntityDoc) extends MatchingError(s"""|LabelAny, no label
                              |Entity ${entity.show()}
                              |""".stripMargin)

  case class NoLang(lang: String, mode: TermMode, entity: EntityDoc) extends MatchingError(s"""|
                                |No value for lang: ${lang}
                              |Entity ${entity.show()}
                              |""".stripMargin)

  case class LabelConstraintNoLang(lang: Lang, entity: EntityDoc)
      extends MatchingError(s"""|No label with lang: ${lang}
                              |Entity ${entity.show()}
                              |""".stripMargin)

  case class DescrAnyNoDescr(entity: EntityDoc) extends MatchingError(s"""|DescrAny, no description
                              |Entity ${entity.show()}
                              |""".stripMargin)

  case class DescriptionConstraintNoLang(lang: Lang, entity: EntityDoc)
      extends MatchingError(s"""|No description with lang: ${lang}
                              |Entity ${entity.show()}
                              |""".stripMargin)

  case class AliasAnyNoAlias(entity: EntityDoc)
      extends MatchingError(s"""|No alias when constraint is any alias
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

  case class WNodeConstraintError(reason: Reason, wdtkValue: WDTKValue, value: Value)
      extends MatchingError(s"""|NodeConstraint Error
                                |reason: $reason
                                |value: $value
                                |wdtkValue: $wdtkValue
                                |""".stripMargin)

  case class WNodeConstraintSnakError(reason: Reason, nc: WNodeConstraint, snak: Snak)
      extends MatchingError(s"""|WNodeConstraint Error
                                |reason: $reason
                                |nodeConstraint: $nc
                                |snak: $snak
                                |""".stripMargin)

  case class StringConstraintError(
      err: StringConstraintMatchError,
      tc: StringConstraint,
      value: MonolingualTextValue
  ) extends MatchingError(s"""|TermConstraint MatchError
                                |StringConstraint: $tc
                                |value: $value
                                |err: ${err}
                              |""".stripMargin)

  case class ReferencesNumLessMin(
      oksCounter: Int,
      min: Int,
      rs: References,
      ref: ReferencesSpecSingle,
      oks: List[Either[MatchingError, Reference]],
      errs: List[Either[MatchingError, Reference]]
  ) extends MatchingError(s"""|Num references match less than min
                                |Num passed: $oks
                                |Min: $min
                                |ref: ${ref}
                              |""".stripMargin)

  case class ReferencesNumGreaterMax(
      oks: Int,
      max: IntOrUnbounded,
      ref: ReferencesSpecSingle
  ) extends MatchingError(s"""|Num references match less than min
                                |Num passed: $oks
                                |Max: $max
                                |ref: $ref
                              |""".stripMargin)

  case class NoMatchingEmptyPropertySpec(
      snaks: List[Snak]
  ) extends MatchingError(s"""|Empty PropertySpec does not match non empty list of snaks
                              |Snaks: $snaks
                              |""".stripMargin)

  case class PropertySpecLocalNumLessMin(
      oksNum: Int,
      min: Int,
      pl: PropertyLocal,
      snaks: List[Snak],
      oks: List[Either[MatchingError, Snak]],
      errs: List[Either[MatchingError, Snak]]
  ) extends MatchingError(s"""|Num properties match less than min
                                |Num passed: $oksNum
                                |Min: $min
                                |PropertyLocal: $pl
                                |snaks: ${snaks}
                                |oks: ${oks}
                                |errs: ${errs}
                                |""".stripMargin)

  case class PropertySpecLocalNumGreaterMax(
      oksNum: Int,
      max: IntOrUnbounded,
      pl: PropertyLocal,
      snaks: List[Snak]
  ) extends MatchingError(s"""|Num references match greater than max
                                |Num passed: $oksNum
                                |Max: $max
                                |PropertyLocal: $pl
                                |snaks: ${snaks}
                              |""".stripMargin)

  case class ShapeNotFound(
      label: ShapeLabel,
      wshex: WSchema
  ) extends MatchingError(s"""|Label ${label} not found in schema
                              |Available labels: [${wshex.labels.map(_.toString).mkString(",")}]
                              |Schema: ${wshex}""".stripMargin)

}
