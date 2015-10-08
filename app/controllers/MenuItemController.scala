package controllers

import models.{MenuItem, MenuItemUtil, JsonData}
import org.xarcher.ubw.modules.DBBase
import play.api._
import play.api.libs.json._
import play.api.libs.functional.syntax._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent._
import utils.JsonCommon._

class MenuItemController extends Controller with DBBase {

  def list = Action.async { implicit request =>
    Future successful Ok(Json.toJson(JsonData(data = Option(MenuItemUtil().menuItems))))
  }

}