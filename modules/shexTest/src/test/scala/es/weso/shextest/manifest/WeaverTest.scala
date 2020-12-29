package es.weso.shextest.manifest

import weaver._
import cats.effect._

object WeaverSuite extends SimpleIOSuite {
 
 val randomUUID = IO(java.util.UUID.randomUUID())

  // A test for side-effecting functions
  simpleTest("hello side-effects") {
    for {
      x <- randomUUID
      y <- randomUUID
    } yield expect(x != y)
  }
}
