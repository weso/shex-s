package es.weso.utils.testsuite
import cats.effect._

// Code taken from: https://typelevel.org/blog/2020/10/30/concurrency-in-ce3.html

trait Latch {
  def release: IO[Unit]
  def await: IO[Unit]
}

sealed trait State
final case class Awaiting(latches: Int, waiter: Deferred[IO, Unit]) extends State
case object Done extends State

object Latch {
  def apply(latches: Int): IO[Latch] =
    for {
      waiter <- IO.deferred[Unit]
      state <- IO.ref[State](Awaiting(latches, waiter))
    } yield new Latch {
      override def release: IO[Unit] = 
        state.modify {
          case Awaiting(n, waiter) => 
            if (n > 1)
              (Awaiting(n - 1, waiter), IO.unit)
            else
              (Done, waiter.complete(()))
          case Done => (Done, IO.unit)
        }.flatten.void
      override def await: IO[Unit] = 
        state.get.flatMap {
          case Done => IO.unit
          case Awaiting(_, waiter) => waiter.get
        }
    }
}
