package org.xarcher.ubw.core

import play.api.libs.json._
import slick.lifted.TupleShape
import scala.annotation.tailrec
import scalaz._, Scalaz._
import scala.language.higherKinds
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions
import org.xarcher.ubw.core.UbwPgDriver.api._

case class UItem(key: String, value: Option[JsValue])
case class UColumn(alias: String/*列的别名*/, describe: String/*列的描述,相当于 select 的列的内容*/, query: String)

trait UContent {

  type ColumnType
  val query: Query[ColumnType, _, Seq]
  def columnGen(rep: ColumnType): PartialFunction[String, Rep[Option[JsValue]]]

}

class UTableQueryContent(val tableName: String) extends UContent {

  override type ColumnType = UbwTable

  override val query = TableQuery(cons => new UbwTable(cons, tableName))

  override def columnGen(rep: ColumnType) = {
    case name: String => (rep.data +> name).?
  }

}

trait UFilter {

  def filter: Map[String, PartialFunction[String, Rep[Option[JsValue]]]] => Rep[Option[Boolean]]

}

class ColumnGt(column: UColumn, num: Long) extends UFilter {

  def filter = map => {
    map(column.query)(column.describe) > Json.toJson(num)
  }

}

trait UColumnMap {

  type ColType <: Product
  type ValType <: Product
  val colMap: Map[String, PartialFunction[String, Rep[Option[JsValue]]]] => ColType //由于要通过表名映射到列操作,所以要多一个 map
  val colReverseMap: ColType => PartialFunction[String, Rep[Option[JsValue]]] //UQuery.toContent 的时候只有一个 content,所以并不需要映射,这里不需要 map
  val dataToList: ValType => List[UItem]
  val shape:  Shape[_ <: FlatShapeLevel, ColType, ValType, ColType]

  def append(column: UColumn)(implicit oldShape: Shape[_ <: FlatShapeLevel, Rep[Option[JsValue]], Option[JsValue], Rep[Option[JsValue]]]): UColumnMap = {
    type ColumnType = Tuple2[ColType, Rep[Option[JsValue]]]
    type ValueType = Tuple2[ValType, Option[JsValue]]
    val tuple2Shape =  new TupleShape[FlatShapeLevel, ColumnType, ValueType, ColumnType](shape, oldShape)
    val columnMap: Map[String, PartialFunction[String, Rep[Option[JsValue]]]] => ColumnType = repMap => {
      val oldColMap = colMap(repMap)
      val genRep = repMap(column.query)(column.describe)
      oldColMap -> genRep
    }
    val columnReverseMap: ColumnType => PartialFunction[String, Rep[Option[JsValue]]] = columns => {
      val oldReverseMap = colReverseMap(columns._1); //分号不可删
      {
        case `column`.`alias` => columns._2
        case s: String => oldReverseMap(s)
      }
    }
    val valueDataToList: ValueType => List[UItem] = value => {
      val content = dataToList(value._1)
      content ::: UItem(column.alias, value._2) :: Nil
    }
    new UColumnMap {
      type ColType = ColumnType
      type ValType = ValueType
      val colMap = columnMap
      val colReverseMap = columnReverseMap
      val dataToList = valueDataToList
      val shape = tuple2Shape
    }
  }
}

trait UQuery {

  val contents: List[(String, UContent)]
  val columns: List[UColumn]
  lazy val columnMap: UColumnMap = UQuery.ouneisangma(columns)

  def kimoji: Query[columnMap.ColType, columnMap.ValType, Seq] = {
    UQuery.mengmengda(contents, columnMap, oneFilter)
  }

  def oneFilter: UFilter

  def toContent = new UContent {
    override type ColumnType = columnMap.ColType
    override val query = kimoji
    override def columnGen(rep: ColumnType) = {
      columnMap.colReverseMap(rep)
    }
  }

  def result(implicit ec: ExecutionContext): DBIO[Seq[Seq[UItem]]] = {
    kimoji.result.map(s => s.map(t => columnMap.dataToList(t)))
  }

}

object UQuery {

  def ouneisangma(queryList: List[UColumn]): UColumnMap = {
    queryList.tail.foldLeft(columnHead(queryList.head))((map, column) => {
      map.append(column)
    })
  }

  private def columnHead(column: UColumn)(implicit oldShape: Shape[_ <: FlatShapeLevel, Rep[Option[JsValue]], Option[JsValue], Rep[Option[JsValue]]]): UColumnMap = {
    type ColumnType = Tuple1[Rep[Option[JsValue]]]
    type ValueType = Tuple1[Option[JsValue]]

    val tuple1Shape = new TupleShape[FlatShapeLevel, ColumnType, ValueType, ColumnType](oldShape)
    val columnMap: Map[String, PartialFunction[String, Rep[Option[JsValue]]]] => ColumnType = repMap => {
      val genRep = repMap(column.query)(column.describe)
      Tuple1(genRep)
    }
    val columnReverseMap: ColumnType => PartialFunction[String, Rep[Option[JsValue]]] = columns => {
      {
        case `column`.`alias` => columns._1
      }
    }
    val valueDataToList: ValueType => List[UItem] = value => List(UItem(column.alias, value._1))
    new UColumnMap {
      type ColType = ColumnType
      type ValType = ValueType
      val colMap = columnMap
      val colReverseMap = columnReverseMap
      val dataToList = valueDataToList
      val shape = tuple1Shape
    }
  }

  def mengmengda(subTQuery: List[(String, UContent)], columnMap: UColumnMap, filter: UFilter, subRepMap: Map[String, PartialFunction[String, Rep[Option[JsValue]]]] = Map()): Query[columnMap.ColType, columnMap.ValType, Seq] = {
    subTQuery match {
      case content :: secondItem :: tail =>
        content._2.query.flatMap(jsRep => {
          val newMap = subRepMap + (content._1 -> content._2.columnGen(jsRep))
          mengmengda(secondItem :: tail, columnMap, filter, newMap)
        })
      case head :: Nil =>
        head._2.query
          .filter(jsRep => {
            val newMap = subRepMap + (head._1 -> head._2.columnGen(jsRep))
            filter.filter(newMap)
          })
          .map(jsRep => {
            val newMap = subRepMap + (head._1 -> head._2.columnGen(jsRep))
            columnMap.colMap(newMap)
          })(columnMap.shape)
      case _ => throw new Exception("喵了个咪,我就是看你不顺眼")
    }
  }

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