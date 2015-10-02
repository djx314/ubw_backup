package org.xarcher.ubw.core

import java.sql.Timestamp

import org.joda.time.DateTime
import play.api.libs.json._
import slick.collection.heterogeneous.HList
import slick.lifted.TupleShape
import scala.annotation.tailrec
import scalaz._, Scalaz._
import scala.language.higherKinds
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions
import org.xarcher.ubw.core.UbwPgDriver.api._

case class UItem(key: String, value: Option[JsValue])
case class UColumn(name: String, describe: String, query: String)

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

trait UColumnMap {

  type ColType <: Product
  type ValType <: Product
  val colMap: Map[String, PartialFunction[String, Rep[Option[JsValue]]]] => ColType
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
    val valueDataToList: ValueType => List[UItem] = value => {
      val content = dataToList(value._1)
      content ::: UItem(column.name, value._2) :: Nil
    }
    new UColumnMap {
      type ColType = ColumnType
      type ValType = ValueType
      val colMap = columnMap
      val dataToList = valueDataToList
      val shape = tuple2Shape
    }
  }
}

trait UQuery {

  val contents: Map[String, UContent]
  val columns: List[UColumn]
  lazy val columnMap: UColumnMap = UQuery.ouneisangma(columns)

  def kimoji: Query[columnMap.ColType, columnMap.ValType, Seq] = {
    UQuery.mengmengda(contents.toList, columnMap, Map.empty[String, PartialFunction[String, Rep[Option[JsValue]]]])
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
    val valueDataToList: ValueType => List[UItem] = value => List(UItem(column.name, value._1))
    new UColumnMap {
      type ColType = ColumnType
      type ValType = ValueType
      val colMap = columnMap
      val dataToList = valueDataToList
      val shape = tuple1Shape
    }
  }

  def mengmengda(subTQuery: List[(String, UContent)], columnMap: UColumnMap, subRepMap: Map[String, PartialFunction[String, Rep[Option[JsValue]]]]): Query[columnMap.ColType, columnMap.ValType, Seq] = {
    subTQuery match {
      case content :: secondItem :: tail =>
        content._2.query.flatMap(jsRep => {
          val newMap = subRepMap + (content._1 -> content._2.columnGen(jsRep))
          mengmengda(secondItem :: tail, columnMap, newMap)
        })
      case head :: Nil =>
        head._2.query.map(jsRep => {
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