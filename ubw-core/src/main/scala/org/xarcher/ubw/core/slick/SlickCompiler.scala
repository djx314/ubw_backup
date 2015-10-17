package org.xarcher.ubw.core.slick

import org.joda.time.DateTime
import play.api.libs.json._
import slick.ast.Library.SqlFunction
import slick.lifted.{CanBeQueryCondition, ColumnOrdered, TupleShape}
import scala.language.higherKinds
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions
import org.xarcher.ubw.core.slick.UbwPgDriver.api._

import org.xarcher.ubw.core.parse._
import org.parboiled2._

import scala.util.{Failure, Success}

case class UItem(key: String, value: Option[JsValue])
case class UColumn(alias: String/*列的别名*/, describe: String/*列的描述,相当于 select 的列的内容*/, query: String)

trait UQuery {

  type ColumnsType
  val query: Query[ColumnsType, _, Seq]
  def columnGen(rep: ColumnsType): PartialFunction[String, Rep[Option[JsValue]]]

}

class UTableQuery(val tableName: String) extends UQuery {

  override type ColumnsType = UbwTable

  override val query = TableQuery(cons => new UbwTable(cons, tableName))

  override def columnGen(rep: ColumnsType) = {
    case name: String => (rep.data +> name).?/*new JsValueRepParser(name, rep.data.?).InputLine.run() match {
      case Success(s) => s
      case Failure(e) =>
        e.printStackTrace
        throw e
    }*/
  }

}

class UColumnGt(column: UColumn, num: Long) extends UBooleanOptFilter {

  //val miaolegemi = SimpleFunction.unary[Option[JsValue], Option[Long]]("convert_to_integer")

  override def repConvert(map: Map[String, PartialFunction[String, Rep[Option[JsValue]]]]): ResultRep = {
    map(column.query)(column.describe) > Json.toJson(num)
    //要 +>>
    //miaolegemi(map(column.query)(column.describe)) > num
  }

}

class UColumnLt(column: UColumn, num: Long) extends UBooleanOptFilter {

  override def repConvert(map: Map[String, PartialFunction[String, Rep[Option[JsValue]]]]): ResultRep = {
    map(column.query)(column.describe) < Json.toJson(num)
  }

}

trait UBooleanOptFilter extends UFilter {

  override type ColumnType = Option[Boolean]

  override val implicitCondition = implicitly[CanBeQueryCondition[ResultRep]]

  def and(other: UBooleanOptFilter) = {
    val selfFilter = this
    new UBooleanOptFilter {
      override def repConvert(map: Map[String, PartialFunction[String, Rep[Option[JsValue]]]]): ResultRep = {
        selfFilter.repConvert(map) && other.repConvert(map)
      }
    }
  }

  def or(other: UBooleanOptFilter) = {
    val selfFilter = this
    new UBooleanOptFilter {
      override def repConvert(map: Map[String, PartialFunction[String, Rep[Option[JsValue]]]]): ResultRep = {
        selfFilter.repConvert(map) || other.repConvert(map)
      }
    }
  }

}

class UColumnLike(column: UColumn, likeString: String) extends UBooleanOptFilter {

  override def repConvert(map: Map[String, PartialFunction[String, Rep[Option[JsValue]]]]): ResultRep = {
    map(column.query)(column.describe).asColumnOf[Option[String]].like(likeString)
  }

}

trait UFilter extends UQueryDeal {

  type ColumnType

  val implicitCondition: CanBeQueryCondition[ResultRep]

  override type ResultRep = Rep[ColumnType]

  override def transform[E, U, C[_]]
  (query: Query[E, U, C])
  (mapConvert: E => Map[String, PartialFunction[String, Rep[Option[JsValue]]]]): Query[E, U, C] = {
    query.filter(s => repConvert(mapConvert(s)))(implicitCondition)
  }

}

class SortBy(column: UColumn, isDesc: Option[Boolean]) extends UQueryDeal {

  override type ResultRep = ColumnOrdered[_]
  override def transform[E, U, C[_]]
  (query: Query[E, U, C])
  (mapConvert: E => Map[String, PartialFunction[String, Rep[Option[JsValue]]]]): Query[E, U, C] = {
    query.sortBy(s => repConvert(mapConvert(s)))
  }

  def repConvert(map: Map[String, PartialFunction[String, Rep[Option[JsValue]]]]): ResultRep = {
    if (isDesc.isDefined) {
      if (isDesc.get)
        map(column.query)(column.describe).desc.nullsLast
      else
        map(column.query)(column.describe).asc.nullsLast
    } else
      map(column.query)(column.describe)

  }

}

trait UQueryDeal {

  type ResultRep

  def transform[E, U, C[_]](query: Query[E, U, C])
    (mapConvert: E => Map[String, PartialFunction[String, Rep[Option[JsValue]]]]): Query[E, U, C]

  def repConvert(map: Map[String, PartialFunction[String, Rep[Option[JsValue]]]]): ResultRep

}

trait UColumnMatch {

  type ColType <: Product
  type ValType <: Product
  val colMap: Map[String, PartialFunction[String, Rep[Option[JsValue]]]] => ColType //由于要通过表名映射到列操作,所以要多一个 map
  val colReverseMap: ColType => PartialFunction[String, Rep[Option[JsValue]]] //UQuery.toContent 的时候只有一个 content,所以并不需要映射,这里不需要 map
  val dataToList: ValType => List[UItem]
  val shape:  Shape[_ <: FlatShapeLevel, ColType, ValType, ColType]

  def append(column: UColumn)(implicit oldShape: Shape[_ <: FlatShapeLevel, Rep[Option[JsValue]], Option[JsValue], Rep[Option[JsValue]]]): UColumnMatch = {
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
    new UColumnMatch {
      type ColType = ColumnType
      type ValType = ValueType
      val colMap = columnMap
      val colReverseMap = columnReverseMap
      val dataToList = valueDataToList
      val shape = tuple2Shape
    }
  }
}

trait UContent {

  val querys: List[(String, UQuery)]
  val columns: List[UColumn]
  def converts: Seq[UQueryDeal]

  lazy val columnMap: UColumnMatch = UContent.genColumnMatch(columns)

  def toSlickQuery: Query[columnMap.ColType, columnMap.ValType, Seq] = {
    UContent.genQuery(querys, columnMap, converts)
  }

  def toContent = new UQuery {
    override type ColumnsType = columnMap.ColType
    override val query = toSlickQuery
    override def columnGen(rep: ColumnsType) = {
      columnMap.colReverseMap(rep)
    }
  }

  def result(implicit ec: ExecutionContext): DBIO[Seq[Seq[UItem]]] = {
    toSlickQuery.result.map(s => s.map(t => columnMap.dataToList(t)))
  }

}

object UContent {

  def genColumnMatch(queryList: List[UColumn]): UColumnMatch = {
    queryList.tail.foldLeft(headColumnMatch(queryList.head))((map, column) => {
      map.append(column)
    })
  }

  private def headColumnMatch(column: UColumn)(implicit oldShape: Shape[_ <: FlatShapeLevel, Rep[Option[JsValue]], Option[JsValue], Rep[Option[JsValue]]]): UColumnMatch = {
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
    new UColumnMatch {
      type ColType = ColumnType
      type ValType = ValueType
      val colMap = columnMap
      val colReverseMap = columnReverseMap
      val dataToList = valueDataToList
      val shape = tuple1Shape
    }
  }

  def genQuery(subTQuery: List[(String, UQuery)], columnMap: UColumnMatch, converts: Seq[UQueryDeal], subRepMap: Map[String, PartialFunction[String, Rep[Option[JsValue]]]] = Map())
  : Query[columnMap.ColType, columnMap.ValType, Seq] = {
    subTQuery match {
      case content :: secondItem :: tail =>
        content._2.query.flatMap(jsRep => {
          val newMap = subRepMap + (content._1 -> content._2.columnGen(jsRep))
          genQuery(secondItem :: tail, columnMap, converts, newMap)
        })
      case head :: Nil =>
        converts
        .foldLeft(head._2.query)((query, convert) => {
          convert
            .transform(query)(jsRep => subRepMap + (head._1 -> head._2.columnGen(jsRep)))
        })
        .map(jsRep => {
          val newMap = subRepMap + (head._1 -> head._2.columnGen(jsRep))
          columnMap.colMap(newMap)
        })(columnMap.shape)
      case _ => throw new Exception("query 的映射不能为空")
    }
  }

}

case class Ubw(
  id: Option[Long] = None,
  data: JsValue,
  updateTime: DateTime = new DateTime()
)

class UbwTable(tag: Tag, tableName: String) extends Table[Ubw](tag, tableName) {

  def id = column[Long]("ID", O.AutoInc, O.PrimaryKey)
  def data = column[JsValue]("DATA")
  def updateTime = column[DateTime]("UPDATE_TIME")

  def * = (id.?, data, updateTime) <> (Ubw.tupled, Ubw.unapply _)

}