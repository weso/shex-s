package es.weso.utils

import cats.kernel.Order
import cats.effect._
import cats.implicits._

sealed abstract class VerboseLevel {
  import VerboseLevel._

  val level: Int
  val name: String
  def asBoolean = this > Basic

  def info(msg: String): IO[Unit] =
    if (this >= Info) IO.println(msg)
    else IO.pure(())

  def basic(msg: String): IO[Unit] =
    if (this >= Basic) IO.println(fansi.Color.Black(msg))
    else IO.pure(())

  def debug(msg: String): IO[Unit] =
    if (this >= Debug) IO.println(fansi.Color.Red(msg))
    else IO.pure(())

  def details(msg: String): IO[Unit] =
    if (this >= Details) IO.println(fansi.Color.DarkGray(msg))
    else IO.pure(())

  def step(msg: String): IO[Unit] =
    if (this >= Step) for {
      _ <- IO.println(msg + "| <press ENTER to continue> ")
      _ <- IO.readLine
    } yield ()
    else IO.pure(())

}

object VerboseLevel {

  case object Nothing extends VerboseLevel {
    val level = 0
    val name  = "nothing"
  }

  case object Basic extends VerboseLevel {
    val level = 1
    val name  = "basic"
  }

  case object Info extends VerboseLevel {
    val level = 2
    val name  = "info"
  }

  case object Details extends VerboseLevel {
    val level = 3
    val name  = "details"
  }

  case object Debug extends VerboseLevel {
    val level = 4
    val name  = "debug"
  }

  case object Step extends VerboseLevel {
    val level = 5
    val name  = "step"
  }

  case object All extends VerboseLevel {
    val level = 6
    val name  = "all"
  }

  val verboseLevels: List[VerboseLevel] = List(Nothing, Basic, Info, Details, Debug, Step, All)

  val showVerboseLevels: String = verboseLevels.map(v => s"${v.level}-${v.name}").mkString(",")

  implicit val OrderVerbose: Order[VerboseLevel] = new Order[VerboseLevel] {
    def compare(x: VerboseLevel, y: VerboseLevel) =
      Order[Int].compare(x.level, y.level)
  }

  def fromString(str: String): Option[VerboseLevel] = {
    val strLower = str.toLowerCase()
    verboseLevels.filter { v =>
      v.level.toString == strLower ||
      v.name.toLowerCase == strLower
    }.headOption
  }

}
