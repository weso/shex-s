package es.weso.shex.extend

import cats.implicits._
import cats._
import munit._

class ExtendMTest extends FunSuite with ExtendM {

  type Label = String
  case class Expr(es: List[Label])
  type Schema = Map[Label, Shape]
  type Cmp[A] = Either[String, A]

  case class Shape(
      extend: Cmp[List[Label]],
      expr: Cmp[Expr]
  ) {

    def flattenExpr(schema: Schema): Cmp[Expr] = {

      implicit val monoidExpr: Monoid[Expr] = new Monoid[Expr] {
       def combine(e1: Expr, e2: Expr): Expr =
        Expr(e1.es ++ e2.es)

       def empty: Expr = Expr(List()) 
      }

      def getEither(lbl: Label): Cmp[Shape] =
        schema.get(lbl).fold(s"Not found".asLeft)(_.asRight)

      extendCheckingVisitedM[Shape, Expr, Label, Cmp](this, getEither(_), _.extend, _.expr)
    }
  }

  def no: Cmp[List[Label]] = List().asRight

  {
    val shape: Shape = Shape(expr = Expr(List("x")).asRight, extend = no)
    val schema: Schema = Map("s" -> shape)
    val expected: Either[String, Expr] = Expr(List("x")).asRight
    shouldFlattenExpr("No extensions", shape, schema, expected)
  }

  { val shape: Shape = Shape(expr = Expr(List("x")).asRight, extend = List("s").asRight)
    val schema: Schema = Map("s" -> shape)
    val expected: Either[String, Expr] = Expr(List("x")).asRight
    shouldFlattenExpr("Circular extension", shape, schema, expected)
  }

  {
    val s: Shape = Shape(extend = List("t").asRight, expr = Expr(List("x")).asRight)
    val t: Shape = Shape(extend = no, expr = Expr(List("y")).asRight)
    val schema: Schema = Map("s" -> s, "t" -> t)
    val expected: Either[String, Expr] = Expr(List("x", "y")).asRight
    shouldFlattenExpr("S extends T", s, schema, expected)
  }

 {
    val s: Shape = Shape(extend = List("r").asRight, expr = Expr(List("x")).asRight)
    val t: Shape = Shape(extend = no, expr = Expr(List("y")).asRight)
    val schema: Schema = Map("s" -> s, "t" -> t)
    val expected: Either[String, Expr] = s"Not found".asLeft
    shouldFlattenExpr("S extends R which doesn't exist (error)", s, schema, expected)
  }

  {
    val s: Shape = Shape(extend = List("t").asRight, expr = Expr(List("x")).asRight)
    val t: Shape = Shape(extend = List("u", "v").asRight, expr = Expr(List("y")).asRight)
    val u: Shape = Shape(extend = no, expr = Expr(List("z1")).asRight)
    val v: Shape = Shape(extend = no, expr = Expr(List("z2")).asRight)
    val schema: Schema = Map("s" -> s, "t" -> t, "u" -> u, "v" -> v)
    val expected: Either[String, Expr] = Expr(List("x", "y", "z1", "z2")).asRight
    shouldFlattenExpr("S extends T, T extends U, V", s, schema, expected)
  }

  {
    val s: Shape = Shape(extend = List("t").asRight, expr = Expr(List("x")).asRight)
    val t: Shape = Shape(extend = List("u", "v").asRight, expr = Expr(List("y")).asRight)
    val u: Shape = Shape(extend = List("t").asRight, expr = Expr(List("z1")).asRight)
    val v: Shape = Shape(extend = no, expr = Expr(List("z2")).asRight)
    val schema: Schema = Map("s" -> s, "t" -> t, "u" -> u, "v" -> v)
    val expected: Either[String, Expr] = Expr(List("x", "y", "z1", "z2")).asRight
    shouldFlattenExpr(
      "S extends T, T extends U, V, U extends T, circular dependency",
      s,
      schema,
      expected
    )
  }

/*    {
    val a: Shape = Shape(expr = Expr(List("p [3]")).some, extend = List("B").some)
    val b: Shape = Shape(expr = Expr(List("p [1]")).some, extend = List().some)
    val schema: Schema = Map("A" -> a, "B" -> b)
    val expected: Either[String, Option[Expr]] = 
      Expr(List("p [3]", "p [1]")).some.asRight
    shouldFlattenExpr(
      "B { p [3]}, A extend B { p [1] } == [p [1], p [3]]",
      a,
      schema,
      expected
    )
  }

    {
    val a: Shape = Shape(expr = Expr(List("p [1]")).some, extend = none)
    val b: Shape = Shape(expr = Expr(List("p [2]")).some, extend = List("A").some)
    val c: Shape = Shape(expr = Expr(List("p [3]")).some, extend = List("A").some)
    val d: Shape = Shape(expr = Expr(List("p [4]")).some, extend = List("B", "C").some)
    val schema: Schema = Map("A" -> a, "B" -> b, "C" -> c, "D" -> d)
    val expected: Either[String, Option[Expr]] = 
      Expr(List("p [4]", "p [3]", "p [2]", "p [1]")).some.asRight
    shouldFlattenExpr(
      "Diamond",
      d,
      schema,
      expected
    )
  } */
  


  def shouldFlattenExpr(
      msg: String,
      s: Shape,
      schema: Schema,
      expected: Either[String,Expr]
  )(implicit loc: munit.Location): Unit =
    test(s"$msg. Should flatten expr of $s and obtain $expected") {
      assertEquals(s.flattenExpr(schema), expected)
    }

}
