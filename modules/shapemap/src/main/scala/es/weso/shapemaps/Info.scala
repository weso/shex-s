package es.weso.shapemaps

import io.circe._
import io.circe.syntax._

case class Info(
  val status: Status = Conformant,
  val reason: Option[String] = None,
  val appInfo: Option[Json] = None)

object Info {
  implicit val encodeInfo: Encoder[Info] = new Encoder[Info] {
    import Status._
    final def apply(i: Info): Json = Json.obj(
      ("status", i.status.asJson),
      ("reason", i.reason.asJson),
      ("appInfo", i.appInfo.asJson))
  }

  def undefined(msg: String): Info =
    Info(Undefined, Some(msg), Some(Json.fromString(msg)))
}
