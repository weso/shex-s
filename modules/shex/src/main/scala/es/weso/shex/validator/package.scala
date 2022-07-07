package es.weso.shex
import es.weso.rbe.Rbe
import es.weso.rbe.BagChecker
import es.weso.collection.Bag

package object validator {

  type Rbe_ = Rbe[ConstraintRef]
  type ConstraintsMap = Map[ConstraintRef, CheckExpr]
  type PathsMap = Map[Path, Set[ConstraintRef]]
  type ResultPair = (CTable, Rbe_)
  type Bag_ = Bag[ConstraintRef]
  type BagChecker_ = BagChecker[ConstraintRef]
  type ES[A] = Either[ShExError, A]

}
