package es.weso.shex.validator
import cats._
import cats.implicits._
import es.weso.rdf.PrefixMap
import es.weso.typing._
import es.weso.rdf.nodes._
import es.weso.shapemaps.{BNodeLabel, IRILabel => IRIMapLabel, _}
import es.weso.shapemaps.Status._
import es.weso.shex.ShapeLabel
import io.circe.Json
import es.weso.shex.shexR.PREFIXES.sx_start
import io.circe._
import io.circe.syntax._
import cats.effect.IO

case class ShapeTyping(
    t: Typing[RDFNode, ShapeType, ShExError, String]
) {

  def showShort(nodesPrefixMap: PrefixMap, shapesPrefixMap: PrefixMap): String = {
    def showPos(ls: Set[ShapeType]): String =
      if (ls.nonEmpty)
        ls.map(st => st.label.map(sl => "+" + shapesPrefixMap.qualify(sl.toRDFNode)).getOrElse(""))
          .mkString(",")
      else " "

    def showNeg(ls: Set[ShapeType]): String =
      if (ls.nonEmpty)
        " " + ls
          .map(st => st.label.map(sl => "-" + shapesPrefixMap.qualify(sl.toRDFNode)).getOrElse(""))
          .mkString(",")
      else ""
    val vs = t.getKeys
      .map(k =>
        (
          nodesPrefixMap.qualify(k),
          s"${showPos(t.getOkValues(k).toSet)}${showNeg(t.getFailedValues(k).toSet)}"
        )
      )
      .map { case (v1, v2) => v1 + "@" + v2 }
      .mkString("| ")
    vs
  }

  def getOkValues(node: RDFNode): Set[ShapeType] =
    t.getOkValues(node).toSet

  def getFailedValues(node: RDFNode): Set[ShapeType] =
    t.getFailedValues(node).toSet

  // TODO Review these definitions in case of anonymous shapes...
  def hasInfoAbout(node: RDFNode, label: ShapeLabel): Boolean =
    hasType(node, label) || hasNoType(node, label)

  def hasType(node: RDFNode, label: ShapeLabel): Boolean =
    getOkValues(node).filter(_.hasLabel(label)).nonEmpty

  def hasNoType(node: RDFNode, label: ShapeLabel): Boolean =
    getFailedValues(node).filter(_.hasLabel(label)).nonEmpty

  def getTypingResult(node: RDFNode, label: ShapeLabel): Option[TypingResult[ShExError, String]] =
    t.getMap.get(node).map(_.toList.filter(_._1.label.contains(label)).map(_._2).head)

  def addType(node: RDFNode, shapeType: ShapeType): ShapeTyping =
    this.copy(t = t.addType(node, shapeType))

  def addEvidence(node: RDFNode, shapeType: ShapeType, evidence: String): ShapeTyping =
    this.copy(t = t.addEvidence(node, shapeType, evidence))

  def addNotEvidence(node: RDFNode, shapeType: ShapeType, err: ShExError): ShapeTyping =
    this.copy(t = t.addNotEvidence(node, shapeType, err))

  def getMap: scala.collection.Map[RDFNode, scala.collection.Map[ShapeType, TypingResult[
    ShExError,
    String
  ]]] =
    t.getMap

  def removeShapeTypesWith(cond: ShapeType => Boolean): ShapeTyping =
    ShapeTyping(t.removeValuesWith(cond))

  def negateShapeTypesWith(cond: ShapeType => Boolean, err: ShExError): ShapeTyping =
    ShapeTyping(t.negateValuesWith(cond, err))

  override def toString: String = showShapeTyping

  private def cnvShapeType(s: ShapeType): Either[String, ShapeMapLabel] = s.label match {
    case None =>
      Left(
        s"Can't create Result shape map for a shape expression without label. ShapeExpr: ${s.se}"
      )
    case Some(lbl) =>
      lbl.toRDFNode match {
        case `sx_start` => Either.right(Start)
        case i: IRI     => Either.right(IRIMapLabel(i))
        case b: BNode   => Either.right(BNodeLabel(b))
        case _ => Left(s"Can't create Result shape map for a shape expression with label: $lbl")
      }
  }

  private def cnvTypingResult(
      t: TypingResult[ShExError, String],
      nodesPrefixMap: PrefixMap,
      shapesPrefixMap: PrefixMap
  ): Info = {
    val status = if (t.isOK) Conformant else NonConformant
    val reason =
      if (t.isOK) t.getEvidences.map(_.mkString("\n"))
      else {
        val s = t.getErrors.map(es =>
          es.map { e =>
            e.showQualified(nodesPrefixMap, shapesPrefixMap)
          // e.msg
          }.mkString("\n")
        )
        s
      }
    val appInfo = ShapeTyping.typingResult2Json(t)
    Info(status, reason, Some(appInfo))
  }

  private def typing2Labels(
      m: collection.Map[ShapeType, TypingResult[ShExError, String]],
      nodesPrefixMap: PrefixMap,
      shapesPrefixMap: PrefixMap
  ): Either[String, Map[ShapeMapLabel, Info]] = {
    def processType(
        m: Either[String, Map[ShapeMapLabel, Info]],
        current: (ShapeType, TypingResult[ShExError, String])
    ): Either[String, Map[ShapeMapLabel, Info]] =
      cnvShapeType(current._1) match {
        case Left(s) =>
          m
        case Right(label) =>
          val info = cnvTypingResult(current._2, nodesPrefixMap, shapesPrefixMap)
          m.map(_.updated(label, info))
      }
    val zero: Either[String, Map[ShapeMapLabel, Info]] = Map[ShapeMapLabel, Info]().asRight[String]
    m.foldLeft(zero)(processType)
  }

  def toShapeMap(
      nodesPrefixMap: PrefixMap,
      shapesPrefixMap: PrefixMap
  ): Either[String, ResultShapeMap] = {

    type Result = Either[String, ResultShapeMap]

    def combine(
        m: Result,
        current: (RDFNode, scala.collection.Map[ShapeType, TypingResult[ShExError, String]])
    ): Result =
      for {
        rm <- m
        ls <- typing2Labels(current._2, nodesPrefixMap, shapesPrefixMap)
      } yield
        if (ls.nonEmpty) rm.addNodeAssociations(current._1, ls)
        else rm

    val zero: Result = ResultShapeMap.empty
      .addNodesPrefixMap(nodesPrefixMap)
      .addShapesPrefixMap(shapesPrefixMap)
      .asRight[String]

    getMap.foldLeft(zero)(combine)
  }

  def showShapeTyping: String = {
    import ShapeTyping._
    t.show
  }

}

object ShapeTyping {

  def emptyShapeTyping: ShapeTyping = {
    val emptyTyping: Typing[RDFNode, ShapeType, ShExError, String] = Typing.empty
    ShapeTyping(emptyTyping)
  }

  implicit lazy val showRDFNode: Show[RDFNode] = new Show[RDFNode] {
    def show(n: RDFNode) = s"$n"
  }

  implicit def showShapeTyping: Show[ShapeTyping] = new Show[ShapeTyping] {
    override def show(t: ShapeTyping): String =
      t.showShapeTyping
  }

  implicit def monoidShapeTyping: Monoid[ShapeTyping] = new Monoid[ShapeTyping] {
    override def empty: ShapeTyping = emptyShapeTyping

    override def combine(t1: ShapeTyping, t2: ShapeTyping): ShapeTyping =
      ShapeTyping(t1.t.combineTyping(t2.t))
  }

  def combineTypings(ts: Seq[ShapeTyping]): ShapeTyping =
    ShapeTyping(
      Typing.combineTypings(ts.map(_.t))
    )

  implicit def showPair: Show[(ShapeTyping, Evidences)] = new Show[(ShapeTyping, Evidences)] {
    def show(e: (ShapeTyping, Evidences)): String =
      s"Typing: ${e._1.show}\n Evidences:\n${e._2.show}"
  }

  def typingResult2Json(t: TypingResult[ShExError, String]): Json =
    if (t.isOK)
      Json.obj(
        ("evidences", Json.fromValues(t.getEvidences.getOrElse(List()).map(Json.fromString(_))))
      )
    else
      Json.obj(("errors", Json.fromValues(t.getErrors.getOrElse(List()).map(_.asJson))))

  implicit def encoderShapeTyping: Encoder[ShapeTyping] = new Encoder[ShapeTyping] {
    implicit lazy val keyEncoderRDFNode: KeyEncoder[RDFNode] = new KeyEncoder[RDFNode] {
      final def apply(p: RDFNode): String = p.show
    }
    implicit lazy val keyEncoderShapeType: KeyEncoder[ShapeType] = new KeyEncoder[ShapeType] {
      final def apply(p: ShapeType): String = p.show
    }
    implicit lazy val typingResultEncoder: Encoder[TypingResult[ShExError, String]] =
      new Encoder[TypingResult[ShExError, String]] {
        final def apply(t: TypingResult[ShExError, String]): Json = typingResult2Json(t)
      }

    final def apply(t: ShapeTyping): Json = {
      val m = t.getMap
      m.asJson
    }
  }

}
