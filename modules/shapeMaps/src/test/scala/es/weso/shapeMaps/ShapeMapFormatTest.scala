package es.weso.shapeMaps
import org.scalatest._

class ShapeMapFormatTest extends FunSpec with Matchers with TryValues with OptionValues {

  describe("ShapeMapFormat") {

    it("should be able to get list of available formats") {
      ShapeMapFormat.availableFormatNames should contain theSameElementsAs(List("compact","json"))
    }

    it(s"Should find a shapeMapFormat if it exists") {
      ShapeMapFormat.fromString("Compact").map(_.name.toLowerCase).getOrElse("failed") should be("compact")
    }

    it(s"Should not find a shapeMapFormat if it does not exist") {
      ShapeMapFormat.fromString("foo").map(_.name.toLowerCase).getOrElse("failed") should be("failed")
    }

  }


}
