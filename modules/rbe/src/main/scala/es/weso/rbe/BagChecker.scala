package es.weso.rbe
import cats.Show
import es.weso.collection._
import cats.data.NonEmptyList

trait BagChecker[A] {

  def rbe: Rbe[A]

  def check(bag: Bag[A], open: Boolean): Either[NonEmptyList[RbeError], Bag[A]]

}

object BagChecker {
  implicit def showBagChecker[A:Show]: Show[BagChecker[A]] =
    new Show[BagChecker[A]] {
      def show(fa: BagChecker[A]): String =
        Rbe.show(fa.rbe)
    }
}