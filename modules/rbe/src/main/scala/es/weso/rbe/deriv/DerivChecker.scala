package es.weso.rbe.deriv
import es.weso.rbe._
import es.weso.collection._
import cats._
import cats.data._
import cats.implicits._

case class DerivChecker[A: Show, Ordering](rbe: Rbe[A]) extends BagChecker[A] {

  def check(bag: Bag[A], open: Boolean): Either[NonEmptyList[RbeError], Bag[A]] =
    rbe.derivBag(bag, open, rbe.symbols) match {
      case f: Fail => NonEmptyList.one(f.error).asLeft
      case d =>
        d.nullable match {
          case Right(_) => Right(bag)
          case Left(m)  => NonEmptyList.one(NonNullableError(d, rbe, bag, open, m)).asLeft
        }
    }
}
