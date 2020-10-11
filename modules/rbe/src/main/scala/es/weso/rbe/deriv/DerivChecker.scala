package es.weso.rbe.deriv
import es.weso.rbe._
import es.weso.collection._
import cats.data._ 
import cats.implicits._

case class DerivChecker[A](rbe: Rbe[A]) extends BagChecker[A] {

  def check(bag: Bag[A], open: Boolean): Either[NonEmptyList[RbeError], Bag[A]] = {
    val d = rbe.derivBag(bag, open, rbe.symbols)
    //    println(s"Deriv of $rbe against $bag = $d")
    if (d.nullable) Right(bag)
    else {
      d match {
        case Fail(e) => Left(NonEmptyList.one(e))
        case _ => NonEmptyList.one(NonNullableError(d, rbe, bag, open)).asLeft
      }
    }
  }

}