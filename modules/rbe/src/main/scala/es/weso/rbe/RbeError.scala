package es.weso.rbe

import scala.util.control.NoStackTrace
// import io.circe.Encoder
import io.circe.Json
import io.circe.syntax._
import es.weso.rbe.interval.Interval
import es.weso.collection.Bag
import es.weso.rbe.interval.IntOrUnbounded
import cats._
import cats.implicits._
import es.weso.rbe.ShowRbe._

sealed abstract class RbeError protected (val msg: String)
 extends Exception(msg) with NoStackTrace with Product with Serializable {
    def show: String
    def toJson: Json
}

case class MsgError(override val msg: String) extends RbeError(msg) {
    override def show: String = s"Error: ${msg}"
    override def toJson: Json = Json.fromString(s"Error: $msg")
}

case class IntervalError[A](
    interval: Interval, 
    rbe: Rbe[A], 
    bag: Bag[A], 
    open: Boolean
 ) extends RbeError(s"""|IntervalChecker Error
                        |Interval ${interval.toString}
                        |Rbe: ${rbe.toString}
                        |Bag: ${bag.toString}
                        |Open?: ${open.toString}
                        |""".stripMargin) {
    override def show: String = msg
    override def toJson: Json = 
       Json.obj(
           ("type", "IntervalCheckerError".asJson),
           ("interval", interval.toString.asJson),
           ("rbe", rbe.toString.asJson),
           ("bag", bag.toString.asJson),
           ("open", open.asJson)
       )
}

case class RepeatsError[A: Show](
    r: Rbe[A], 
    rbe: Rbe[A], 
    bag: Bag[A]
  ) extends RbeError(s"Repeats error for ${rbe}. Repeated = ${r}") {
    override def show: String = s"""|Interval algorithm applied to a Rbe with repeats
                                    |RBe: ${rbe}
                                    |Repeats: ${r}
                                    |Bag: ${bag}
                                    |""".stripMargin

    override def toJson: Json = Json.obj(
      ("type", "RepeatError".asJson),
      ("repeat", r.toString.asJson),
      ("rbe", rbe.toString.asJson),
      ("bag", bag.toString.asJson),
    )                                    
}

case class NonNullableError[A:Show](
    nonNullable: Rbe[A], 
    rbe: Rbe[A], 
    bag: Bag[A], 
    open: Boolean
 ) extends RbeError(s"""|NonNullable: ${nonNullable.show}
                        |Rbe: ${rbe.show}
                        |Bag: ${bag.toString}
                        |Open?: ${open.toString}
                        |""".stripMargin) {
    override def show: String = msg
    override def toJson: Json = 
       Json.obj(
           ("type", "NonNullableError".asJson),
           ("interval", nonNullable.show.asJson),
           ("rbe", rbe.show.asJson),
           ("bag", bag.toString.asJson),
           ("open", open.asJson)
       )
}

case class RangeNegativeLowerBound(
    m: Int
 ) extends RbeError(s"""|Range negative lower bound: ${m}
                        |""".stripMargin) {
    override def show: String = msg
    override def toJson: Json = 
       Json.obj(
           ("type", "RangeNegativeLowerBound".asJson),
           ("value", m.asJson),
       )
}

case class RangeLowerBoundBigger(
    m: Int,
    n: IntOrUnbounded
 ) extends RbeError(s"""|Range lower bound ${m} bigger than ${n}
                        |""".stripMargin) {
    override def show: String = msg
    override def toJson: Json = 
       Json.obj(
           ("type", "RangeLowerBoundBigger".asJson),
           ("m", m.asJson),
           ("n", n.asJson),
       )
}

case class MaxCardinalityZeroFoundValue[A](
    x: A,
    s: Symbol[A]
 ) extends RbeError(s"""|Found x=${x} but max cardinality = 0 for ${s}
                        |""".stripMargin) {
    override def show: String = msg
    override def toJson: Json = 
       Json.obj(
           ("type", "MaxCardinaolityZeroFoundValue".asJson),
           ("x", x.toString.asJson),
           ("s", s.toString.asJson),
       )
}

case class Unexpected[A:Show](
    x: A,
    s: Rbe[A],
    open: Boolean
 ) extends RbeError(s"""|Unexpected ${x.show} doesn't match ${s.show}\nOpen?: $open
                        |""".stripMargin) {
    override def show: String = msg
    override def toJson: Json = 
       Json.obj(
           ("type", "Unexpected".asJson),
           ("found", x.show.asJson),
           ("expected", s.show.asJson),
           ("open", open.asJson),
       )
}

case class UnexpectedEmpty[A:Show](
    x: A,
    open: Boolean
 ) extends RbeError(s"""|Unexpected ${x.show} doesn't match empty. Open?: $open
                        |""".stripMargin) {
    override def show: String = msg
    override def toJson: Json = 
       Json.obj(
           ("type", "Unexpected".asJson),
           ("x", x.show.asJson),
           ("open", open.asJson),
       )
}

case class CardinalityZeroZeroDeriv[A:Show](
    x: A,
    e: Rbe[A],
    d: Rbe[A],
 ) extends RbeError(s"""|Cardinality 0,0 but deriv e=${e.show}/x=${x.show} = $d is nullable
                        |""".stripMargin) {
    override def show: String = msg
    override def toJson: Json = 
       Json.obj(
           ("type", "CardinalityZeroZeroDerivNullable".asJson),
           ("x", x.show.asJson),
           ("e", e.show.asJson),
           ("d", d.show.asJson),
       )
}