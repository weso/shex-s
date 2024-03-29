package es.weso.rbe.interval

import es.weso.collection._
import es.weso.rbe._
import munit._

class IntervalTest extends FunSuite with BagMatchers {

  val emptyBag: Bag[String] = Bag.toBag(List())

  equalInterval(Empty, Bag.toBag(List("a", "b", "a")), Interval(0, Unbounded))

  equalInterval(Symbol("a", 0, 1), Bag.toBag(List("b")), Interval(0, Unbounded))

  equalInterval(Symbol("a", 0, 1), Bag.toBag(List("a", "b")), Interval(1, Unbounded))

  equalInterval(Symbol("a", 0, 1), Bag.toBag(List("a", "b", "a")), Interval(2, Unbounded))

  equalInterval(Symbol("a", 1, 2), Bag.toBag(List("a", "b", "a")), Interval(1, 2))

  equalInterval(Symbol("a", 2, 4), Bag.toBag(List("a", "b", "a")), Interval(1, 1))

  equalInterval(Symbol("a", 2, 3), Bag.toBag(List("a", "b", "a")), Interval(1, 1))

  equalInterval(Symbol("a", 2, 400), Bag.toBag(List("a", "b", "a")), Interval(1, 1))

  equalInterval(Symbol("a", 1, 1), Bag.toBag(List("a", "b", "a")), Interval(2, 2))

  equalInterval(
    Symbol("a", 2, 2),
    Bag.toBag(List("a", "b", "a", "a", "a", "a", "a")),
    Interval(3, 3)
  )

  equalInterval(
    Symbol("b", 2, 2),
    Bag.toBag(List("a", "b", "a", "a", "a", "a", "a")),
    Interval(1, 0)
  )

  equalInterval(
    Symbol("b", 1, 1),
    Bag.toBag(List("a", "b", "a", "a", "a", "a", "a")),
    Interval(1, 1)
  )

  equalInterval(
    Symbol("c", 1, 1),
    Bag.toBag(List("a", "b", "a", "a", "a", "a", "a")),
    Interval(0, 0)
  )

  equalInterval(
    And(Symbol("a", 1, 2), Symbol("b", 1, 1)),
    Bag.toBag(List("a", "b", "a")),
    Interval(1, 1)
  )

  equalInterval(
    And(Symbol("a", 1, 2), Symbol("b", 2, 2)),
    Bag.toBag(List("a", "b", "a")),
    Interval(1, 0)
  )

  equalInterval(Symbol("a", 0, 0), Bag.toBag(List("a", "b")), Interval(Unbounded, Unbounded))

  equalInterval(Symbol("a", 0, 0), Bag.toBag(List("b")), Interval(0, Unbounded))

  equalInterval(Symbol("b", 0, 0), Bag.toBag(List("b")), Interval(Unbounded, Unbounded))

  equalInterval(Plus(Symbol("a", 1, 1)), Bag.toBag(List("a", "a", "a")), Interval(1, 3))

  equalInterval(Plus(Symbol("a", 1, 1)), Bag.toBag(List("b", "b", "c")), Interval(0, 0))

  equalInterval(Star(Symbol("a", 1, 1)), Bag.toBag(List("a", "a", "a")), Interval(1, Unbounded))

  equalInterval(Star(Symbol("a", 1, 1)), Bag.toBag(List("b", "b", "c")), Interval(0, Unbounded))

  equalInterval(And(Symbol("a", 1, 1), Symbol("b", 1, 1)), Bag.toBag(List("a")), Interval(1, 0))

  equalInterval(And(Symbol("a", 1, 1), Symbol("b", 1, 1)), Bag.toBag(List("b")), Interval(1, 0))

  equalInterval(And(Symbol("a", 1, 1), Symbol("b", 1, 1)), emptyBag, Interval(0, 0))

  equalInterval(And(Symbol("a", 1, 1), And(Symbol("b", 1, 1), Empty)), emptyBag, Interval(0, 0))

  equalInterval(And(Symbol("a", 1, 1), Empty), emptyBag, Interval(0, 0))

  equalInterval(Or(Symbol("a", 1, 1), Symbol("b", 1, 1)), emptyBag, Interval(0, 0))

  equalInterval(
    Star(
      And(And(Symbol("a", 1, 1), Plus(Star(Symbol("d", 2, 2)))), Star(Star(Symbol("b", 20, 20))))
    ),
    Bag("a", "a", "a", "a", "b", "b", "b", "b", "c", "d", "e", "e"),
    Interval(4, 0)
  )

  /*    equalInterval(
        And(Repeat(And(Symbol("a",1,1),Symbol("b",1,1)),0,1), Symbol("c",1,1)),
        Bag.toBag(List("c")), Interval(1,1))

    equalInterval(
        And(Repeat(And(Symbol("a",1,1),Symbol("b",1,1)),0,1), Symbol("c",1,1)),
        Bag.toBag(List("b", "c")), Interval(1,1))

    equalInterval(
        And(Repeat(And(Symbol("a",1,1),Symbol("b",1,1)),0,1), Symbol("c",1,1)),
        Bag.toBag(List("a", "c")), Interval(1,1))

    equalInterval(
        And(Repeat(And(Symbol("a",1,1),Symbol("b",1,1)),0,1), Symbol("c",1,1)),
        Bag.toBag(List("a", "a", "c")), Interval(2,1))

    equalInterval(
        And(Repeat(And(Symbol("a",1,1),Symbol("b",1,1)),0,1), Symbol("c",1,1)),
        Bag.toBag(List("a")), Interval(1,0))

    equalInterval(
        And(And(And(Symbol("a",1,1),Symbol("b",1,1)),
                And(Symbol("a",1,1),Symbol("b",1,1))
            )
            , Symbol("c",1,1)),
        Bag.toBag(List("a","b","a","b","c")), Interval(1,1))

    equalInterval(
        And(Repeat(And(Symbol("a",1,1),Symbol("b",1,1)),3,5), Symbol("c",1,1)),
        Bag.toBag(List("a","b","a","b","a","b","c")), Interval(1,1))

    equalInterval(
        And(Repeat(And(Symbol("a",1,1),Symbol("b",1,1)),3,5), Symbol("c",1,1)),
        Bag.toBag(List("a","b","a","b","a","b","a","b","a","b","c")), Interval(1,1))

    // Fail...
    equalInterval(
        And(Repeat(And(Symbol("a",1,1),Symbol("b",1,1)),3,5), Symbol("c",1,1)),
        Bag.toBag(List("a","b","a","b","a","b","a","b","a","b","a","b","c")), Interval(1,1))

    equalInterval(
        And(Repeat(And(Symbol("a",1,1),Symbol("b",1,1)),3,Unbounded), Symbol("c",1,1)),
        Bag.toBag(List("a","a","a","c")), Interval(1,1))

    equalInterval(
        And(Repeat(And(Symbol("a",1,1),Symbol("b",1,1)),3,Unbounded), Symbol("c",1,1)),
        Bag.toBag(List("a","b","a","b","a","b","a","b","c")), Interval(1,1))
   */
  containsBag(Empty, Bag.toBag(List("a", "b", "a")))
  containsBag(Symbol("a", 1, 2), Bag.toBag(List("a", "b", "a")))
  containsBag(Symbol("a", 2, 4), Bag.toBag(List("a", "b", "a")))
  containsBag(Symbol("a", 2, 3), Bag.toBag(List("a", "b", "a")))
  containsBag(Symbol("a", 2, 400), Bag.toBag(List("a", "b", "a")))
  notContainsBag(Symbol("a", 1, 1), Bag.toBag(List("a", "b", "a")))
  notContainsBag(Symbol("a", 2, 2), Bag.toBag(List("a", "b", "a", "a", "a", "a", "a")))
  notContainsBag(Symbol("b", 2, 2), Bag.toBag(List("a", "b", "a", "a", "a", "a", "a")))
  containsBag(Symbol("b", 1, 1), Bag.toBag(List("a", "b", "a", "a", "a", "a", "a")))
  notContainsBag(Symbol("c", 1, 1), Bag.toBag(List("a", "b", "a", "a", "a", "a", "a")))
  containsBag(And(Symbol("a", 1, 2), Symbol("b", 1, 1)), Bag.toBag(List("a", "b", "a")))
  notContainsBag(And(Symbol("a", 1, 2), Symbol("c", 1, 1)), Bag.toBag(List("a", "b", "a")))
  containsBag(Or(Symbol("a", 1, 2), Symbol("c", 1, 1)), Bag.toBag(List("a", "b", "a")))
  notContainsBag(Or(Symbol("a", 3, 5), Symbol("c", 1, 1)), Bag.toBag(List("a", "b", "a")))
  notContainsBag(Or(Symbol("a", 0, 1), Symbol("c", 1, 1)), Bag.toBag(List("a", "b", "a")))
  notContainsBag(Symbol("a", 0, 0), Bag.toBag(List("a", "b", "a")))
  notContainsBag(Symbol("a", 0, 0), Bag.toBag(List("a", "b")))

  containsBag(Symbol("a", 0, 0), Bag.toBag(List("b")))

  notContainsBag(Symbol("b", 0, 0), Bag.toBag(List("b")))

  containsBag(Or(Symbol("a", 0, 0), Symbol("b", 1, 1)), Bag.toBag(List("b")))
  containsBag(Or(Symbol("a", 0, 0), Symbol("b", 1, 1)), Bag.toBag(List("c")))
  containsBag(Or(Symbol("a", 0, 0), Symbol("b", 1, 1)), Bag.toBag(List("b", "c")))
  notContainsBag(Or(Symbol("a", 0, 0), Symbol("b", 1, 1)), Bag.toBag(List("a", "b")))
  containsBag(Or(Symbol("a", 1, 1), Symbol("b", 1, 1)), Bag.toBag(List("b", "c")))
  notContainsBag(Or(Symbol("a", 1, 1), Symbol("b", 1, 1)), Bag.toBag(List("a", "b")))
  equalInterval(Or(Symbol("a", 1, 1), Symbol("b", 1, 1)), Bag.toBag(List("a", "b")), Interval(2, 2))
  notContainsBag(Or(Symbol("a", 1, 1), Symbol("b", 1, 1)), Bag.toBag(List("a", "b", "c")))
  containsBag(Or(Symbol("a", 2, 2), Symbol("b", 1, 1)), Bag.toBag(List("a", "a", "c")))
  notContainsBag(Or(Symbol("a", 2, 2), Symbol("b", 1, 1)), Bag.toBag(List("a", "a", "c")), false)

  matchBag(Symbol("a", 1, 1), Bag.toBag(List("a")))
  matchBag(Symbol("a", 1, 1), Bag.toBag(List("a", "b")), true)
  noMatchBag(Symbol("a", 1, 1), Bag.toBag(List("a", "b")), false)
  matchBag(Empty, Bag.toBag(List("a", "b", "a")))
  matchBag(Symbol("a", 1, 2), Bag.toBag(List("a", "b", "a")))
  matchBag(Symbol("a", 2, 4), Bag.toBag(List("a", "b", "a")))
  matchBag(Symbol("a", 2, 3), Bag.toBag(List("a", "b", "a")))
  matchBag(Symbol("a", 2, 400), Bag.toBag(List("a", "b", "a")))
  noMatchBag(Symbol("a", 1, 1), Bag.toBag(List("a", "b", "a")))
  noMatchBag(Symbol("a", 2, 2), Bag.toBag(List("a", "b", "a", "a", "a", "a", "a")))
  noMatchBag(Symbol("b", 2, 2), Bag.toBag(List("a", "b", "a", "a", "a", "a", "a")))
  matchBag(Symbol("b", 1, 1), Bag.toBag(List("a", "b", "a", "a", "a", "a", "a")))
  noMatchBag(Symbol("c", 1, 1), Bag.toBag(List("a", "b", "a", "a", "a", "a", "a")))
  matchBag(And(Symbol("a", 1, 2), Symbol("b", 1, 1)), Bag.toBag(List("a", "b", "a")))
  noMatchBag(And(Symbol("a", 1, 2), Symbol("c", 1, 1)), Bag.toBag(List("a", "b", "a")))
  matchBag(Or(Symbol("a", 1, 2), Symbol("c", 1, 1)), Bag.toBag(List("a", "b", "a")))
  noMatchBag(Or(Symbol("a", 3, 5), Symbol("c", 1, 1)), Bag.toBag(List("a", "b", "a")))
  noMatchBag(Or(Symbol("a", 0, 1), Symbol("c", 1, 1)), Bag.toBag(List("a", "b", "a")))
  noMatchBag(Symbol("a", 0, 0), Bag.toBag(List("a", "b", "a")))
  noMatchBag(Symbol("a", 0, 0), Bag.toBag(List("a", "b")))
  matchBag(Symbol("a", 0, 0), Bag.toBag(List("b")))
  noMatchBag(Symbol("b", 0, 0), Bag.toBag(List("b")))
  matchBag(Or(Symbol("a", 0, 0), Symbol("b", 1, 1)), Bag.toBag(List("b")))
  matchBag(Or(Symbol("a", 0, 0), Symbol("b", 1, 1)), Bag.toBag(List("c")))
  matchBag(Or(Symbol("a", 0, 0), Symbol("b", 1, 1)), Bag.toBag(List("b", "c")))
  noMatchBag(Or(Symbol("a", 0, 0), Symbol("b", 1, 1)), Bag.toBag(List("a", "b")))
  matchBag(Or(Symbol("a", 1, 1), Symbol("b", 1, 1)), Bag.toBag(List("b", "c")))
  noMatchBag(Or(Symbol("a", 1, 1), Symbol("b", 1, 1)), Bag.toBag(List("a", "b")))
  noMatchBag(Or(Symbol("a", 1, 1), Symbol("b", 1, 1)), Bag.toBag(List("a", "b", "c")))
  matchBag(Or(Symbol("a", 2, 2), Symbol("b", 1, 1)), Bag.toBag(List("a", "a", "c")))
  noMatchBag(Or(Symbol("a", 2, 2), Symbol("b", 1, 1)), Bag.toBag(List("a", "a", "c")), false)
  matchBag(
    Repeat(And(Symbol("a", 1, 1), Symbol("b", 1, 1)), 1, Unbounded),
    Bag.toBag(List("a", "b", "c"))
  )
  matchBag(
    And(Repeat(And(Symbol("a", 1, 1), Symbol("b", 1, 1)), 1, Unbounded), Symbol("c", 1, 1)),
    Bag.toBag(List("a", "b", "c"))
  )
  noMatchBag(
    And(Repeat(And(Symbol("a", 1, 1), Symbol("b", 1, 1)), 1, Unbounded), Symbol("c", 1, 1)),
    Bag.toBag(List("a", "b", "a", "c"))
  )
  noMatchBag(
    And(Repeat(And(Symbol("a", 1, 1), Symbol("b", 1, 1)), 1, Unbounded), Symbol("c", 1, 1)),
    Bag.toBag(List("a", "b", "b", "c"))
  )
  matchBag(
    And(Repeat(And(Symbol("a", 1, 1), Symbol("b", 1, 1)), 1, Unbounded), Symbol("c", 1, 1)),
    Bag.toBag(List("a", "b", "a", "b", "c"))
  )
  noMatchBag(
    And(Repeat(And(Symbol("a", 1, 1), Symbol("b", 1, 1)), 1, Unbounded), Symbol("c", 1, 1)),
    Bag.toBag(List("a", "b", "a", "b", "b", "c"))
  )
  matchBag(
    And(Repeat(And(Symbol("a", 1, 1), Symbol("b", 1, 1)), 1, Unbounded), Symbol("c", 1, 1)),
    Bag.toBag(List("a", "b", "a", "b", "c", "d"))
  )
  noMatchBag(Repeat(Symbol("a", 1, 1), 0, 0), Bag.toBag(List("a")))
  matchBag(Repeat(Symbol("a", 1, 1), 0, 0), Bag.toBag(List("b")))
  noMatchBag(Repeat(Or(Symbol("a", 1, 1), Symbol("b", 1, 1)), 0, 0), Bag.toBag(List("a")))
  noMatchBag(Repeat(Or(Symbol("a", 1, 1), Symbol("b", 1, 1)), 0, 0), Bag.toBag(List("b")))
  matchBag(Repeat(Or(Symbol("a", 1, 1), Symbol("b", 1, 1)), 0, 0), Bag.toBag(List("c")))

}
