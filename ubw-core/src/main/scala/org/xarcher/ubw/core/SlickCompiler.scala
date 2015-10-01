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

case class UTable(tableName: String)
case class UQuery(query: Query[Rep[JsValue], JsValue, Seq])
case class UColumn(repName: String, columnGen: Rep[JsValue] => Rep[JsValue])

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
    UColumn("bcd", column => column +> "bcd"),
    UColumn("abc", column => column +> "ddd" +> "eee")
  )

  def miaolegemi(tQuery: List[(String, Query[Rep[JsValue], JsValue, Seq])], queryMap: (UColumn, UColumn, UColumn), repMap: Map[String, Rep[JsValue]])
  : Query[(Rep[JsValue], Rep[JsValue], Rep[JsValue]), (JsValue, JsValue, JsValue), Seq] = {
    tQuery match {
      case content :: secondItem :: tail =>
        content._2.flatMap(jsRep => {
          val newMap = repMap + (content._1 -> jsRep)
          miaolegemi(secondItem :: tail, queryMap, newMap)
        })
      case head :: Nil =>
        head._2.map(jsRep => {
          val newMap = repMap + (head._1 -> jsRep)
          val (query1, query2, query3) = queryMap
          val rep1 = query1.columnGen(newMap(query1.repName))
          val rep2 = query2.columnGen(newMap(query2.repName))
          val rep3 = query3.columnGen(newMap(query3.repName))
          (rep1, rep2, rep3)
        })
      case _ => throw new Exception("喵了个咪,我就是看你不顺眼")
    }
  }

  def run: Query[(Rep[JsValue], Rep[JsValue], Rep[JsValue]), (JsValue, JsValue, JsValue), Seq] = {
    miaolegemi(tQueryMap.toList, (queryMap(0), queryMap(1), queryMap(2)), Map.empty[String, Rep[JsValue]])
  }

  def run1111(implicit ec: ExecutionContext): DBIO[Seq[Seq[JsValue]]] = {
    xiaomai(tQueryMap.toList, queryMap)
  }

  def xiaomai(tQuery: List[(String, Query[Rep[JsValue], JsValue, Seq])], queryList: List[UColumn])
  (implicit ec: ExecutionContext)
  : DBIO[Seq[Seq[JsValue]]] = {
    val columnMap = ouneisangma(queryList)

    def mengmengda(subTQuery: List[(String, Query[Rep[JsValue], JsValue, Seq])], subRepMap: Map[String, Rep[JsValue]]): Query[columnMap.ColType, columnMap.ValType, Seq] = {
      subTQuery match {
        case content :: secondItem :: tail =>
          content._2.flatMap(jsRep => {
            val newMap = subRepMap + (content._1 -> jsRep)
            mengmengda(secondItem :: tail, newMap)
          })
        case head :: Nil =>
          head._2.map(jsRep => {
            val newMap = subRepMap + (head._1 -> jsRep)
            println(columnMap.colMap(newMap))
            columnMap.colMap(newMap)
          })(columnMap.shape)
        case _ => throw new Exception("喵了个咪,我就是看你不顺眼")
      }
    }

    val query = mengmengda(tQuery, Map.empty[String, Rep[JsValue]])
    println(1234.toString * 100)
    println(query.asInstanceOf[Query[columnMap.ColType, columnMap.ValType, Seq]])
    query.result.map(s => s.map(t => columnMap.dataToList(t)))
  }

  def ouneisangma(queryList: List[UColumn]): ColumnMap = {
    queryList.foldLeft(columnHead(queryList.head))((map, column) => {
      map.append(column)
    })
  }

  private def columnHead(column: UColumn)(implicit oldShape: Shape[_ <: FlatShapeLevel, Rep[JsValue], JsValue, Rep[JsValue]]): ColumnMap = {
    type ColumnType = Tuple1[Rep[JsValue]]
    type ValueType = Tuple1[JsValue]

    val tuple1Shape = new TupleShape[FlatShapeLevel, ColumnType, ValueType, ColumnType](oldShape)
    val columnMap: Map[String, Rep[JsValue]] => ColumnType = repMap => {
      val rep = repMap(column.repName)
      val genRep = column.columnGen(rep)
      Tuple1(genRep)
    }
    val valueDataToList: ValueType => List[JsValue] = value => List(value._1)
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
    val colMap: Map[String, Rep[JsValue]] => ColType
    val dataToList: ValType => List[JsValue]
    val shape:  Shape[_ <: FlatShapeLevel, ColType, ValType, ColType]

    def append(column: UColumn)(implicit oldShape: Shape[_ <: FlatShapeLevel, Rep[JsValue], JsValue, Rep[JsValue]]): ColumnMap = {
      type ColumnType = Tuple2[ColType, Rep[JsValue]]
      type ValueType = Tuple2[ValType, JsValue]
      val tuple2Shape =  new TupleShape[FlatShapeLevel, ColumnType, ValueType, ColumnType](shape, oldShape)
      val columnMap: Map[String, Rep[JsValue]] => ColumnType = repMap => {
        val oldColMap = colMap(repMap)
        val rep = repMap(column.repName)
        val genRep = column.columnGen(rep)
        oldColMap -> genRep
      }
      val valueDataToList: ValueType => List[JsValue] = value => {
        val content = dataToList(value._1)
        content ::: value._2 :: Nil
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