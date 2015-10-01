package org.xarcher.ubw.core

import java.sql.Timestamp

import org.joda.time.DateTime
import play.api.libs.json._
import scalaz._, Scalaz._
import scala.language.higherKinds
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions
import org.xarcher.ubw.core.UbwPgDriver.api._

case class UTable(tableName: String)
case class UQuery(query: Query[Rep[JsValue], JsValue, Seq])
case class UColumn(queryName: String, colunmGen: Rep[JsValue] => Rep[JsValue])(implicit val dynShape: Shape[_ <: ShapeLevel, Rep[JsValue], JsValue, Rep[JsValue]])

trait SlickCompiler {
  /**
   * select "abc"."bbb", "bcd"."ccc", "abc"."ddd"."eee" from "abc", "bcd" where "abc"."bbb" = 2 and "abc"."bcd_id" = "bcd"."id"
   */
  def genTq(tName: String) = {
    object tableQuery extends TableQuery(cons => new UbwTable(cons, tName))
    tName -> tableQuery
  }

  val tNameMap: Map[String, TableQuery[UbwTable]] = Map(
    genTq("abc"),
    genTq("bcd")
  )

  val tQueryMap: Map[String, Query[Rep[JsValue], JsValue, Seq]] =
    tNameMap.map { case (key, tableQuery) => {
      key -> tableQuery.map(_.data)
    } }

  val queryMap = List(
    UColumn("abc", column => column +> "bbb"),
    UColumn("bbb", column => column +> "bcd"),
    UColumn("abc", column => column +> "ddd" +> "eee")
  )

}

case class Ubw(
  id: Option[Long] = None,
  data: JsValue
)

class UbwTable(tag: Tag, tableName: String) extends Table[Ubw](tag, tableName) {

  def id = column[Long]("ID", O.AutoInc, O.PrimaryKey)
  def data = column[JsValue]("DATA")

  def * = (id.?, data) <> (Ubw.tupled, Ubw.unapply _)

}