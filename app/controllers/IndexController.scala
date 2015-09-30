package controllers

import org.xarcher.ubw.modules.DBBase
import play.api._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent._

class IndexController extends Controller with DBBase {

  import driver.api._
  import play.api.libs.json._

  def index = Action.async { implicit request =>
    Future successful Ok(views.html.index())
  }

  def javascriptRoutes = Action.async { implicit request =>
    Future successful Ok(
      routing.JavaScriptReverseRouter("jsRoutes")(
        routes.javascript.MenuItemController.list
      )
    ).as("text/javascript")
  }

}