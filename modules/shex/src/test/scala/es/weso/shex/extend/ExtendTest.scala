package es.weso.shex.extend

import cats.implicits._
import cats._
import munit._

class ExtendTest extends FunSuite with Extend {

  type Label = String
  case class Expr(es: List[Label])
  type Schema = Map[Label, Shape]

  case class Shape(
      extend: Option[List[Label]],
      expr: Option[Expr]
  ) {

    def flattenExpr(schema: Schema): Either[String, Option[Expr]] = {

      implicit val semigroupExpr: Semigroup[Expr] = new Semigroup[Expr] {
       def combine(e1: Expr, e2: Expr): Expr =
        Expr(e1.es ++ e2.es)
      }

      def getEither(lbl: Label): Either[String, Shape] =
        schema.get(lbl).fold(Either.left[String, Shape](s"Not found"))(Either.right(_))

      extendCheckingVisited[Shape, Expr, String, Label](this, getEither(_), _.extend, _.expr)
    }
  }

  {
    val shape: Shape = Shape(expr = Expr(List("x")).some, extend = none)
    val schema: Schema = Map("s" -> shape)
    val expected: Either[String, Option[Expr]] = Right(Some(Expr(List("x"))))
    shouldFlattenExpr("No extensions", shape, schema, expected)
  }

  { val shape: Shape = Shape(expr = Expr(List("x")).some, extend = List("s").some)
    val schema: Schema = Map("s" -> shape)
    val expected: Either[String, Option[Expr]] = Right(Some(Expr(List("x"))))
    shouldFlattenExpr("Circular extension", shape, schema, expected)
  }

  {
    val s: Shape = Shape(Some(List("t")), Some(Expr(List("x"))))
    val t: Shape = Shape(None, Some(Expr(List("y"))))
    val schema: Schema = Map("s" -> s, "t" -> t)
    val expected: Either[String, Option[Expr]] = Right(Some(Expr(List("x", "y"))))
    shouldFlattenExpr("S extends T", s, schema, expected)
  }

  {
    val s: Shape = Shape(Some(List("r")), Some(Expr(List("x"))))
    val t: Shape = Shape(None, Some(Expr(List("y"))))
    val schema: Schema = Map("s" -> s, "t" -> t)
    val expected: Either[String, Option[Expr]] = Left(s"Not found")
    shouldFlattenExpr("S extends R which doesn't exist (error)", s, schema, expected)
  }

  {
    val s: Shape = Shape(Some(List("t")), Some(Expr(List("x"))))
    val t: Shape = Shape(Some(List("u", "v")), Some(Expr(List("y"))))
    val u: Shape = Shape(None, Some(Expr(List("z1"))))
    val v: Shape = Shape(None, Some(Expr(List("z2"))))
    val schema: Schema = Map("s" -> s, "t" -> t, "u" -> u, "v" -> v)
    val expected: Either[String, Option[Expr]] = Right(Some(Expr(List("x", "y", "z2", "z1"))))
    shouldFlattenExpr("S extends T, T extends U, V", s, schema, expected)
  }

  {
    val s: Shape = Shape(Some(List("t")), Some(Expr(List("x"))))
    val t: Shape = Shape(Some(List("u", "v")), Some(Expr(List("y"))))
    val u: Shape = Shape(Some(List("t")), Some(Expr(List("z1"))))
    val v: Shape = Shape(None, Some(Expr(List("z2"))))
    val schema: Schema = Map("s" -> s, "t" -> t, "u" -> u, "v" -> v)
    val expected: Either[String, Option[Expr]] = Right(Some(Expr(List("x", "y", "z2", "z1"))))
    shouldFlattenExpr(
      "S extends T, T extends U, V, U extends T, circular dependency",
      s,
      schema,
      expected
    )
  }

    {
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
  }
  


  def shouldFlattenExpr(
      msg: String,
      s: Shape,
      schema: Schema,
      expected: Either[String, Option[Expr]]
  )(implicit loc: munit.Location): Unit =
    test(s"$msg. Should flatten expr of $s and obtain $expected") {
      assertEquals(s.flattenExpr(schema), expected)
    }

}
