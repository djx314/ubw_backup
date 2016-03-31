package net.scalax.ubw.mapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._

import scala.concurrent.ExecutionContext
import slick.dbio._

case class SlickRange(drop: Long, take: Option[Long])
case class SlickPage(pageIndex: Long, pageSize: Long)
case class ColumnOrder(columnName: String, isDesc: Boolean)

case class SlickParam(orders: List[ColumnOrder] = Nil, range: Option[SlickRange] = None, page: Option[SlickPage] = None)

case class DataGen(list: () => List[SlickData], map: () => Map[String, SlickData])
case class ResultGen(data: List[DataGen], sum: Long)
case class PropertyInfo(property: String, typeName: String, isHidden: Boolean, canOrder: Boolean, isDefaultDesc: Boolean)
case class QueryInfo(properties: List[PropertyInfo], dataGen: SlickParam => DBIO[ResultGen]) {

  //lazy val properties: List[PropertyInfo] = wrapper.properties

  def toTableData(param: SlickParam = SlickParam())(implicit ec: ExecutionContext): DBIO[TableData] = dataGen(param).map(s =>
    TableData(
      properties = this.properties,
      data = s.data.map(t => t.map().map { case (key, value) => key -> value.toJson }),
      sum = s.sum
    )
  )

  def toTableData(columnNames: List[(String, Boolean)], drop: Long, take: Option[Long])(implicit ec: ExecutionContext): DBIO[TableData] = {
    val orders = columnNames.map(s => ColumnOrder(columnName = s._1, isDesc = s._2))
    toTableData(SlickParam(orders = orders, range = Option(SlickRange(drop, take))))
  }

}
case class TableData(properties: List[PropertyInfo], data: List[Map[String, Json]], sum: Long)