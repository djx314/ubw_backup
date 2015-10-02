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

case class UColumn(columnKey: String, repName: String, columnGen: Rep[Option[JsValue]] => Rep[Option[JsValue]])
case class UItem(key: String, value: Option[JsValue])

/*trait UContent {

  def tra

}*/

trait SlickCompiler {
  /**
   * select "abc"."bbb", "bcd"."ccc", "abc"."ddd"."eee" from "abc", "bcd" where "abc"."bbb" = 2 and "abc"."bcd_id" = "bcd"."id"
   */

  def xiaomai(tQuery: List[(String, Query[Rep[Option[JsValue]], Option[JsValue], Seq])], queryList: List[UColumn])
  (implicit ec: ExecutionContext)
  : DBIO[Seq[Seq[UItem]]] = {

    val columnMap = ouneisangma(queryList)

    def mengmengda(subTQuery: List[(String, Query[Rep[Option[JsValue]], Option[JsValue], Seq])], subRepMap: Map[String, Rep[Option[JsValue]]]): Query[columnMap.ColType, columnMap.ValType, Seq] = {
      subTQuery match {
        case content :: secondItem :: tail =>
          content._2.flatMap(jsRep => {
            val newMap = subRepMap + (content._1 -> jsRep)
            mengmengda(secondItem :: tail, newMap)
          })
        case head :: Nil =>
          head._2.map(jsRep => {
            val newMap = subRepMap + (head._1 -> jsRep)
            columnMap.colMap(newMap)
          })(columnMap.shape)
        case _ => throw new Exception("喵了个咪,我就是看你不顺眼")
      }
    }

    val query = mengmengdamiao(columnMap, tQuery, Map.empty[String, Rep[Option[JsValue]]])
    val query1 = mengmengda(tQuery, Map.empty[String, Rep[Option[JsValue]]])
    query1.result.map(s => s.map(t => columnMap.dataToList(t)))
  }

  def mengmengdamiao(columnMap: ColumnMap, subTQuery: List[(String, Query[Rep[Option[JsValue]], Option[JsValue], Seq])], subRepMap: Map[String, Rep[Option[JsValue]]])
    : Query[columnMap.ColType, columnMap.ValType, Seq] = {
    subTQuery match {
      case content :: secondItem :: tail =>
        content._2.flatMap(jsRep => {
          val newMap = subRepMap + (content._1 -> jsRep)
          mengmengdamiao(columnMap, secondItem :: tail, newMap)
        })
      case head :: Nil =>
        head._2.map(jsRep => {
          val newMap = subRepMap + (head._1 -> jsRep)
          columnMap.colMap(newMap)
        })(columnMap.shape)
      case _ => throw new Exception("喵了个咪,我就是看你不顺眼")
    }
  }

  def ouneisangma(queryList: List[UColumn]): ColumnMap = {
    queryList.tail.foldLeft(columnHead(queryList.head))((map, column) => {
      map.append(column)
    })
  }

  private def columnHead(column: UColumn)(implicit oldShape: Shape[_ <: FlatShapeLevel, Rep[Option[JsValue]], Option[JsValue], Rep[Option[JsValue]]]): ColumnMap = {
    type ColumnType = Tuple1[Rep[Option[JsValue]]]
    type ValueType = Tuple1[Option[JsValue]]

    val tuple1Shape = new TupleShape[FlatShapeLevel, ColumnType, ValueType, ColumnType](oldShape)
    val columnMap: Map[String, Rep[Option[JsValue]]] => ColumnType = repMap => {
      val rep = repMap(column.repName)
      val genRep = column.columnGen(rep)
      Tuple1(genRep)
    }
    val valueDataToList: ValueType => List[UItem] = value => List(UItem(column.columnKey, value._1))
    new ColumnMap {
      type ColType = ColumnType
      type ValType = ValueType
      val colMap = columnMap
      val dataToList = valueDataToList
      val shape = tuple1Shape
    }
  }

  trait ColumnMap {
    type ColType <: Product
    type ValType <: Product
    val colMap: Map[String, Rep[Option[JsValue]]] => ColType
    val dataToList: ValType => List[UItem]
    val shape:  Shape[_ <: FlatShapeLevel, ColType, ValType, ColType]

    def append(column: UColumn)(implicit oldShape: Shape[_ <: FlatShapeLevel, Rep[Option[JsValue]], Option[JsValue], Rep[Option[JsValue]]]): ColumnMap = {
      type ColumnType = Tuple2[ColType, Rep[Option[JsValue]]]
      type ValueType = Tuple2[ValType, Option[JsValue]]
      val tuple2Shape =  new TupleShape[FlatShapeLevel, ColumnType, ValueType, ColumnType](shape, oldShape)
      val columnMap: Map[String, Rep[Option[JsValue]]] => ColumnType = repMap => {
        val oldColMap = colMap(repMap)
        val rep = repMap(column.repName)
        val genRep = column.columnGen(rep)
        oldColMap -> genRep
      }
      val valueDataToList: ValueType => List[UItem] = value => {
        val content = dataToList(value._1)
        content ::: UItem(column.columnKey, value._2) :: Nil
      }
      new ColumnMap {
        type ColType = ColumnType
        type ValType = ValueType
        val colMap = columnMap
        val dataToList = valueDataToList
        val shape = tuple2Shape
      }
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