package es.weso.shex.validator.validatorref

import cats._
import cats._
import implicits._
import cats.effect._
import es.weso.shex._
import es.weso.rdf._
import es.weso.rdf.nodes._
import es.weso.rbe.interval.IntervalChecker
import es.weso.rbe.Empty
import es.weso.utils.SetUtils
import es.weso.shex.implicits.showShEx._
import es.weso.shapemaps.{
  BNodeLabel => BNodeMapLabel,
  IRILabel => IRIMapLabel,
  Start => StartMapLabel,
  _
}
import es.weso.shex.actions.TestSemanticAction
import Function.tupled
// import es.weso.shex.validator.ShExError._
import es.weso.shex.validator.ConstraintRef.{showConstraintRef => _}
import es.weso.utils.internal.CollectionCompat._
import es.weso.utils.VerboseLevel
import es.weso.shex.validator.ExternalResolver
import es.weso.shex.validator.Validator
import es.weso.shex.validator.ShowValidator
import es.weso.shex.validator.Result
import es.weso.shex.validator.ValidationLog
import es.weso.shex._
import es.weso.shex.validator.ShExError._
import alleycats.std.set._
import es.weso.shapemaps.Status._

/** ShEx validator with global state using ref
  */
case class ValidatorRef(
    schema: ResolvedSchema,
    externalResolver: ExternalResolver = ExternalResolver.NoAction,
    builder: RDFBuilder
) extends Validator
    with ShowValidator {

  // Public methods

  /** Validate a node against the START declaration
    */
  def validateNodeStart(rdf: RDFReader, node: IRI, verbose: VerboseLevel): IO[Result] =
    validateNodeShape(rdf, node, "Start", verbose)

  /** Validate a node following target declarations.
    * This methods follows SHACL convention and could be deprecated in the future
    */
  def validateNodeDecls(rdf: RDFReader, verbose: VerboseLevel): IO[Result] =
    IO.raiseError(StringError("Not implemented validateNodeDecls"))

  /** Validate a node against a shape
    */
  def validateNodeShape(
      rdf: RDFReader,
      node: IRI,
      shape: String,
      verbose: VerboseLevel
  ): IO[Result] =
    rdf.getPrefixMap.flatMap(prefixMap =>
      ShapeMap
        .fromString(s"${node.show}@${shape}", "Compact", None, prefixMap, schema.prefixMap)
        .fold(
          e => IO.raiseError(StringError(s"Error parsing shapeMap: $e")),
          sm =>
            ShapeMap
              .fixShapeMap(sm, rdf, prefixMap, schema.prefixMap)
              .flatMap(fm => validateShapeMap(rdf, fm, verbose))
        )
    )

  /** Validate a node against a shape map
    */
  def validateShapeMap(
      rdf: RDFReader,
      shapeMap: FixedShapeMap,
      verbose: VerboseLevel
  ): IO[Result] = {
    val startState = State.fromFixedMap(shapeMap)
    Ref[IO]
      .of(startState)
      .flatMap(refState =>
        verbose.debug(s"Validator with Ref: shapeMap: ${shapeMap.showShapeMap(false)}") *>
          Monad[IO].whileM_(morePending(refState))(evaluatePending(refState)) *>
          getResult(refState)
      )
  }

  private def getState(refState: Ref[IO, State]): IO[State] = refState.get

  private def morePending(refState: Ref[IO, State]): IO[Boolean] =
    getState(refState).flatMap(state => state.pending.nonEmpty.pure[IO])

  private def evaluatePending(refState: Ref[IO, State]): IO[Unit] =
    getState(refState).flatMap(state =>
      IO.println(s"Pending evaluations: ${state.pending}") *>
        state.pending.parFoldMapA { case (node, label, info) =>
          validateNodeShapePending(refState, node, label, info)
        }
    )

  private def validateNodeShapePending(
      refState: Ref[IO, State],
      node: RDFNode,
      shapeLabel: ShapeMapLabel,
      info: Info
  ): IO[Unit] =
    refState
      .updateAndGet(_.changePending(node, shapeLabel, info))
      .flatMap(newState =>
        newState.shapeMap
          .get(node)
          .fold(
            validateNodeShape(refState)(node, shapeLabel)
          ) { case infoMap =>
            infoMap
              .get(shapeLabel)
              .fold(
                validateNodeShape(refState)(node, shapeLabel)
              ) { case info =>
                info.status match {
                  case PendingConforms => validateNodeShape(refState)(node, shapeLabel)
                  case _               => ().pure[IO]
                }
              }
          }
      )

  private def validateNodeShape(
      refState: Ref[IO, State]
  )(node: RDFNode, shapeMapLabel: ShapeMapLabel): IO[Unit] = {
    val shapeLabel = ShapeLabel.fromShapeMapLabel(shapeMapLabel)
    schema
      .getShape(shapeLabel)
      .fold(
        e => IO.raiseError(StringError(s"ShapeLabel $shapeLabel not found")),
        se => validateNodeShapeExpr(refState, node, shapeLabel, se)
      )
  }

  private def validateNodeShapeExpr(
      refState: Ref[IO, State],
      node: RDFNode,
      shapeLabel: ShapeLabel,
      se: ShapeExpr
  ): IO[Unit] = se match {
    case nc: NodeConstraint => validateNodeConstraint(refState, node, nc)
    case _ => IO.raiseError(StringError(s"validateNodeShapeExpr: not implemented ${se}"))
  }

  private def validateNodeConstraint(
      refState: Ref[IO, State],
      node: RDFNode,
      nc: NodeConstraint
  ): IO[Unit] =
    NodeConstraintValidator
      .validateNodeConstraint(node, nc)
      .flatMap(r =>
        if (r.isValid)
          IO.println(s"$node conforms to node constraint $nc")
        else
          IO.println(s"$node doesn't conform to $nc")
      )

  private def getResult(refState: Ref[IO, State]): IO[Result] =
    getState(refState).flatMap { state =>
      val vlog: ValidationLog = ValidationLog(List(), List())
      val rsm: ResultShapeMap = ResultShapeMap(
        state.shapeMap,
        nodesPrefixMap = state.nodesPrefixMap,
        shapesPrefixMap = state.shapesPrefixMap
      )
      var r: Result = Result((vlog, rsm).asRight)
      r.pure[IO]
    }
}

object ValidatorRef {

  /*  def apply(schema: ResolvedSchema, externalResolver: ExternalResolver, builder: RDFBuilder): Validator =
    ValidatorRef(schema, externalResolver, builder) */

}
