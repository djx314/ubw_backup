package org.xarcher.ubw.wrapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import scala.reflect.runtime.universe._
import slick.dbio._
import slick.driver.{JdbcProfile, JdbcActionComponent}
import slick.lifted._

/**
  * Created by djx314 on 15-5-24.
  */
trait SlickData {

  val property: String
  type DataType
  val data: DataType

  val jsonEncoder: Encoder[DataType]

  def toJson: Json = {
    data.asJson(jsonEncoder)
  }

  val typeTag: WeakTypeTag[DataType]

  override def toString = s"SlickData(property=$property,data=$data,type=${typeTag.tpe})"

}

trait SqlFilter[S] {

  type ResultType <: Rep[_]
  type TableType = S
  val wt: CanBeQueryCondition[ResultType]
  val convert: TableType => ResultType

}

trait SqlRepOrder[TargetType] {

  type RepType
  val typeConvert: TargetType <:< Rep[RepType]
  val wt: Rep[RepType] => ColumnOrdered[RepType]

}

trait SqlOrder[S] {

  type RepType
  type TableType = S
  val wt: RepType => Ordered
  val convert: TableType => RepType

}

trait SqlGroupBy[S] {

  type RepType
  type TableType = S
  type T
  type G

  val convert: TableType => RepType

  val kshape: Shape[_ <: FlatShapeLevel, RepType, T, G]
  val vshape: Shape[_ <: FlatShapeLevel, TableType, _, TableType]

}

sealed trait SqlRepBase[S, R, G, T] {

  //type T
  //type G
  val proName: String
  val isHidden: Boolean
  val isDefaultDesc: Boolean
  val shape: Shape[_ <: FlatShapeLevel, R, T, G]
  val valueTypeTag: WeakTypeTag[T]
  val jsonEncoder: Encoder[T]
  /**
    * 如果同时拥有 orderTarget 和 ordereImplicit，以 orderTarget 为先
    */
  val orderTargetName: Option[String] = None
  val sqlOrder: Option[SqlRepOrder[G]] = None

  def hidden(isHidden: Boolean = this.isHidden): this.type

  def order[K](isDefaultDesc: Boolean)(implicit columnGen: G <:< Rep[K], wtImplicit: Rep[K] => ColumnOrdered[K]): this.type

  def orderTarget(targetName: String, isDefaultDesc: Boolean): this.type

}

trait SqlRep[S, R, G, T] extends SqlRepBase[S, R, G, T] {

  val f: S => R

  def asQ[H, P, N](convert: Query[G, T, Seq] => N)(implicit mikuShape: Shape[_ <: FlatShapeLevel, N, P, H], jsonEncoder1: Encoder[P], valueTypeTag1: WeakTypeTag[P]) = {

    val f1 = this.f

    val proName1 = this.proName
    val isHidden1 = this.isHidden
    val isDefaultDesc1 = this.isDefaultDesc
    val mshape = this.shape
    //val orderTargetName1 = this.orderTargetName
    //val sqlOrder1 = this.sqlOrder

    new SqlGRep[S, N, H, P] {
      override type U = R
      override type B = T
      override type W = G
      override val f = f1
      override val ushape = mshape
      override val e = convert
      override val valueTypeTag = valueTypeTag1
      override val jsonEncoder = jsonEncoder1
      //这 2 个都是 none，记得
      override val orderTargetName = None
      override val sqlOrder = None
      override val proName = proName1
      override val isHidden = isHidden1
      override val isDefaultDesc = isDefaultDesc1
      override val shape = mikuShape
    }
  }

  override def hidden(isHidden: Boolean = this.isHidden): this.type = {
    val isHidden1 = isHidden
    this.copy(isHidden = isHidden1)
  }

  override def order[K](isDefaultDesc: Boolean)(implicit columnGen: G <:< Rep[K], wtImplicit: Rep[K] => ColumnOrdered[K]): this.type = {
    val isDefaultDesc1 = isDefaultDesc
    val sqlOrder1 = new SqlRepOrder[G] {
      override type RepType = K
      override val typeConvert = columnGen
      override val wt = wtImplicit
    }
    this.copy(sqlOrder = Option(sqlOrder1), isDefaultDesc = isDefaultDesc1)
  }

  override def orderTarget(targetName: String, isDefaultDesc: Boolean): this.type = {
    val targetName1 = targetName
    val isDefaultDesc1 = isDefaultDesc
    this.copy(orderTargetName = Option(targetName1), isDefaultDesc = isDefaultDesc1)
  }

  def copy(proName: String = this.proName, isHidden: Boolean = this.isHidden, isDefaultDesc: Boolean = this.isDefaultDesc, f: S => R = this.f,
           orderTargetName: Option[String] = this.orderTargetName, sqlOrder: Option[SqlRepOrder[G]] = this.sqlOrder): this.type = {
    type R1 = R
    type T1 = T
    type G1 = G
    val proName1 = proName
    val isHidden1 = isHidden
    val isDefaultDesc1 = isDefaultDesc
    val f1 = f
    val shape1 = this.shape
    val valueTypeTag1 = this.valueTypeTag
    val jsonEncoder1 = this.jsonEncoder
    val orderTargetName1 = orderTargetName
    val sqlOrder1 = sqlOrder
    new SqlRep[S, R, G1, T1] {
      //override type T = T1
      //override type G = G1
      override val proName = proName1
      override val isHidden = isHidden1
      override val isDefaultDesc = isDefaultDesc1
      override val f = f1
      override val shape = shape1
      override val valueTypeTag = valueTypeTag1
      override val jsonEncoder = jsonEncoder1
      override val orderTargetName = orderTargetName1
      override val sqlOrder = sqlOrder1
    }.asInstanceOf[this.type]
  }

}

trait SqlGRep[S, R, G, T] extends SqlRepBase[S, R, G, T] {

  type U
  type B
  type W

  val f: S => U
  val ushape: Shape[_ <: FlatShapeLevel, U, B, W]
  //val shape: Shape[_ <: FlatShapeLevel, R, T, G]
  val e: Query[W, B, Seq] => R

  val queryToRep: Query[S, _, Seq] => R = (query) => {
    val query1 = query.map(xarcher => f(xarcher))(ushape)
    e(query1)
  }

  override def hidden(isHidden: Boolean = this.isHidden): this.type = {
    val isHidden1 = isHidden
    this.copy(isHidden = isHidden1)
  }

  override def order[K](isDefaultDesc: Boolean)(implicit columnGen: G <:< Rep[K], wtImplicit: Rep[K] => ColumnOrdered[K]): this.type = {
    val isDefaultDesc1 = isDefaultDesc
    val sqlOrder1 = new SqlRepOrder[G] {
      override type RepType = K
      override val typeConvert = columnGen
      override val wt = wtImplicit
    }
    this.copy(sqlOrder = Option(sqlOrder1), isDefaultDesc = isDefaultDesc1)
  }

  override def orderTarget(targetName: String, isDefaultDesc: Boolean): this.type = {
    val targetName1 = targetName
    val isDefaultDesc1 = isDefaultDesc
    this.copy(orderTargetName = Option(targetName1), isDefaultDesc = isDefaultDesc1)
  }

  def copy(proName: String = this.proName, isHidden: Boolean = this.isHidden, isDefaultDesc: Boolean = this.isDefaultDesc,
           f: S => U = this.f, e: Query[W, B, Seq] => R = this.e,
           orderTargetName: Option[String] = this.orderTargetName, sqlOrder: Option[SqlRepOrder[G]] = this.sqlOrder): this.type = {
    type U1 = U
    type B1 = B
    type W1 = W
    type R1 = R
    type T1 = T
    type G1 = G
    val proName1 = proName
    val isHidden1 = isHidden
    val isDefaultDesc1 = isDefaultDesc
    val f1 = f

    val ushape1 = ushape
    val e1 = e

    val shape1 = this.shape
    val valueTypeTag1 = this.valueTypeTag
    val jsonEncoder1 = this.jsonEncoder
    val orderTargetName1 = orderTargetName
    val sqlOrder1 = sqlOrder
    new SqlGRep[S, R, G, T1] {
      override type U = U1
      override type B = B1
      override type W = W1
      //override type T = T1
      //override type G = G1
      override val proName = proName1
      override val isHidden = isHidden1
      override val isDefaultDesc = isDefaultDesc1
      override val f = f1

      override val ushape = ushape1
      override val e = e1

      override val shape = shape1
      override val valueTypeTag = valueTypeTag1
      override val jsonEncoder = jsonEncoder1
      override val orderTargetName = orderTargetName1
      override val sqlOrder = sqlOrder1
    }.asInstanceOf[this.type]
  }

}

case class SlickRange(drop: Long, take: Long)
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

  def toTableData(columnNames: List[(String, Boolean)], drop: Long, take: Long)(implicit ec: ExecutionContext): DBIO[TableData] = {
    val orders = columnNames.map(s => ColumnOrder(columnName = s._1, isDesc = s._2))
    toTableData(SlickParam(orders = orders, range = Option(SlickRange(drop, take))))
  }

}
case class TableData(properties: List[PropertyInfo], data: List[Map[String, Json]], sum: Long)