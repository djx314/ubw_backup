package controllers

import models.{MenuItem, MenuItemUtil, JsonData}
import org.joda.time.DateTime
import org.xarcher.ubw.modules.DBBase
import play.api._
import play.api.libs.json._
import play.api.mvc._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import scala.concurrent._
import utils.JsonCommon._
import play.api.data.Form
import play.api.data.Forms._
import models._

import scala.util.{Failure, Success}

class DataCreateController extends Controller with DBBase {

  import driver.api._

  val uploadForm = Form(
    single(
      "miaolegemi" -> text
    )
  )

  def list = Action.async { implicit request =>
    Future successful Ok(views.html.dataCreate("")("请输入表名"))
  }

  def create = Action.async(parse.form(uploadForm)) { implicit request =>
    val tableName = request.body
    object tableQuery extends TableQuery(cons => new UbwTable(cons, tableName))
    val insertFuture = db.run(tableQuery += Ubw(data = Json.obj("小aa缘bb喵cc" -> ("我dd是ee小ff缘gg喵" + new DateTime().toString), "日期" -> new DateTime().toString)))
    val valuesFuture = db.run(tableQuery.map(s => s.data +> "小aa缘bb喵cc").result)
    valuesFuture.flatMap { values =>
      db.run(tableQuery.result).flatMap { ubws =>
        Future successful Ok(views.html.dataCreateSuccess(tableName)(ubws)(true)(values))
      }
    } recoverWith { case e => {
      println(e.getMessage)
      db.run(tableQuery.schema.create.transactionally).map(_ => {
        Ok(views.html.dataCreate(tableName)("表已新建,请再次提交"))
      })
    } }
  }

}