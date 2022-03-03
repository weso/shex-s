package es.weso.shex

/**
  * Represents the maximum cardinality which can be a number or unbounded
  */
abstract sealed trait Max {

  def show = this match {
    case IntMax(v) => v.toString
    case Star => "*"
  }

  def biggerThanOrEqual(x: Int) = this match {
    case IntMax(v) => v >= x
    case Star => true
  }

  def decreaseCard: Max = this match {
    case Star => Star
    case IntMax(0) => IntMax(0)
    case IntMax(n) if n > 0 => IntMax(n - 1)
    case n => throw new Exception(s"DecreaseCard: Unexpected negative value of max card: $n")
  }

}

/**
  * Unbounded cardinality
  */
case object Star extends Max

/**
  * Bounded cardinality represented by an Int value
  *
  * @param v maximum cardinality
  */
case class IntMax(v: Int) extends Max
