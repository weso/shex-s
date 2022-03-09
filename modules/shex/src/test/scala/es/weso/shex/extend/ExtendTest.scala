package es.weso.shex.extend

import cats.syntax.either._
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

      def combine(e1: Expr, e2: Expr): Expr =
        Expr(e1.es ++ e2.es)

      def getEither(lbl: Label): Either[String, Shape] =
        schema.get(lbl).fold(Either.left[String, Shape](s"Not found"))(Either.right(_))

      extendCheckingVisited[Shape, Expr, Label](this, getEither(_), _.extend, combine, _.expr)
    }
  }

  {
    val shape: Shape                           = Shape(None, Some(Expr(List("x"))))
    val schema: Schema                         = Map("s" -> shape)
    val expected: Either[String, Option[Expr]] = Right(Some(Expr(List("x"))))
    shouldFlattenExpr("No extensions", shape, schema, expected)
  }

  { // TODO: Not sure about the expected result
    val shape: Shape                           = Shape(Some(List("s")), Some(Expr(List("x"))))
    val schema: Schema                         = Map("s" -> shape)
    val expected: Either[String, Option[Expr]] = Right(Some(Expr(List("x", "x"))))
    shouldFlattenExpr("Circular extension", shape, schema, expected)
  }

  {
    val s: Shape                               = Shape(Some(List("t")), Some(Expr(List("x"))))
    val t: Shape                               = Shape(None, Some(Expr(List("y"))))
    val schema: Schema                         = Map("s" -> s, "t" -> t)
    val expected: Either[String, Option[Expr]] = Right(Some(Expr(List("y", "x"))))
    shouldFlattenExpr("S extends T", s, schema, expected)
  }

  {
    val s: Shape                               = Shape(Some(List("r")), Some(Expr(List("x"))))
    val t: Shape                               = Shape(None, Some(Expr(List("y"))))
    val schema: Schema                         = Map("s" -> s, "t" -> t)
    val expected: Either[String, Option[Expr]] = Left(s"Not found")
    shouldFlattenExpr("S extends R which doesn't exist (error)", s, schema, expected)
  }

  {
    val s: Shape                               = Shape(Some(List("t")), Some(Expr(List("x"))))
    val t: Shape                               = Shape(Some(List("u", "v")), Some(Expr(List("y"))))
    val u: Shape                               = Shape(None, Some(Expr(List("z1"))))
    val v: Shape                               = Shape(None, Some(Expr(List("z2"))))
    val schema: Schema                         = Map("s" -> s, "t" -> t, "u" -> u, "v" -> v)
    val expected: Either[String, Option[Expr]] = Right(Some(Expr(List("z2", "z1", "y", "x"))))
    shouldFlattenExpr("S extends T, T extends U,V", s, schema, expected)
  }
  {
    val s: Shape                               = Shape(Some(List("t")), Some(Expr(List("x"))))
    val t: Shape                               = Shape(Some(List("u", "v")), Some(Expr(List("y"))))
    val u: Shape                               = Shape(Some(List("t")), Some(Expr(List("z1"))))
    val v: Shape                               = Shape(None, Some(Expr(List("z2"))))
    val schema: Schema                         = Map("s" -> s, "t" -> t, "u" -> u, "v" -> v)
    val expected: Either[String, Option[Expr]] = Right(Some(Expr(List("z2", "z1", "y", "x"))))
    shouldFlattenExpr("S extends T, T extends U,V, U extends T, circular dependency", s, schema, expected)
  }

  def shouldFlattenExpr(msg: String, s: Shape, schema: Schema, expected: Either[String, Option[Expr]])(implicit
      loc: munit.Location
  ): Unit = {
    test(s"$msg. Should flatten expr of $s and obtain $expected") {
      assertEquals(s.flattenExpr(schema), expected)
    }
  }

}
