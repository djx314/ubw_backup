package models

import play.api.libs.json.JsValue
import play.api.mvc.{AnyContent, Request}
import scala.annotation.tailrec
import utils.UbwKeys.FileType
import org.xarcher.ubw.modules.UbwPostgresDriver.api._

case class Ubw(
  id: Option[Long] = None,
  data: JsValue
)

class UbwTable(tag: Tag, tableName: String) extends Table[Ubw](tag, tableName) {

  def id = column[Long]("ID", O.AutoInc, O.PrimaryKey)
  def data = column[JsValue]("DATA")

  def * = (id.?, data) <> (Ubw.tupled, Ubw.unapply _)

}