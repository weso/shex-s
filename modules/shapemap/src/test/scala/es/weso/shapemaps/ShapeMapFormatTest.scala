package es.weso.shapemaps
import munit._

class ShapeMapFormatTest extends FunSuite {

  test("should be able to get list of available formats") {
    assertEquals(ShapeMapFormat.availableFormatNames, List("compact", "json", "details"))
  }

  test(s"Should find a shapeMapFormat if it exists") {
    assertEquals(ShapeMapFormat.fromString("Compact").map(_.name.toLowerCase).getOrElse("failed"), "compact")
  }

  test(s"Should not find a shapeMapFormat if it does not exist") {
    assertEquals(ShapeMapFormat.fromString("foo").map(_.name.toLowerCase).getOrElse("failed"), "failed")
  }

}
