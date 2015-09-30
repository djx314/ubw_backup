package controllers

import models.{MenuItem, MenuItemUtil, JsonData}
import org.xarcher.ubw.modules.DBBase
import play.api._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent._

class MenuItemController extends Controller with DBBase {

  implicit val formatMenuItem = Json.format[MenuItem]

  implicit class PlayJsonFormatOps[T](f: Format[T]) {
    /**
     * Replace the serializer for an object with an optional property to make it return an actual JsNull,
     * instead of omitting the property as it does by default
     *
     * @param key Key that contains the property to be strictified
     * @param get Function that can be used to retrieve the optional property from the object
     */
    def strictNull[V: Format](key: String, get: T => Option[V]): Format[T] = new Format[T] {

      def reads(j: JsValue) = f.reads(j)

      // replace the value in order to provide an explicit json null
      def writes(u: T): JsValue =
        f.writes(u) match {
          case jsObject: JsObject =>
            (jsObject - key) ++ Json.obj(
              key -> (get(u).map(implicitly[Format[V]].writes _).getOrElse(JsNull): JsValue)
            )
          case jsValue => jsValue
        }


    }
  }

  implicit def jsonDataFormat[T](implicit fmt: Format[T]): Format[JsonData[T]] = {
    val format: OFormat[JsonData[T]] = ((__ \ "data").formatNullable[T](fmt) ~
      (__ \ "pageIndex").formatNullable[Int] ~
      (__ \ "pageSize").formatNullable[Int] ~
      (__ \ "sum").formatNullable[Int] ~
      (__ \ "message").formatNullable[String] ~
      (__ \ "isSuccessed").format[Boolean]
      )(JsonData.apply, unlift(JsonData.unapply))
    format.strictNull("data", _.data).strictNull("pageIndex", _.pageIndex).strictNull("pageSize", _.pageSize).strictNull("sum", _.sum).strictNull("message", _.message)
  }

  def list = Action.async { implicit request =>
    Future successful Ok(Json.toJson(JsonData(data = Option(MenuItemUtil().menuItems))))
  }

}