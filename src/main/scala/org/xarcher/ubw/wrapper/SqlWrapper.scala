package org.xarcher.ubw.wrapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._
import slick.ast.TypedType

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import slick.dbio._
import slick.driver.JdbcActionComponent
import slick.lifted._
import scala.reflect.runtime.universe._

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

trait SqlOrder[S] {

  type ValType
  type TableType = S
  val wt: Rep[ValType] => ColumnOrdered[ValType]
  val convert: TableType => Rep[ValType]

}

trait SqlRep[S, R] {
  //type +RepType = R
  type T
  type G
  val proName: String
  val isHidden: Boolean = false
  val f: S => R
  val shape: Shape[_ <: FlatShapeLevel, R, T, G]
  val valueTypeTag: WeakTypeTag[T]
  val jsonEncoder: Encoder[T]
  /**
    * 如果同时拥有 orderTarget 和 ordereImplicit，以 orderTarget 为先
    */
  val orderTarget: Option[String] = None
  val sqlOrder: Option[SqlOrder[S]] = None

  def hidden(isHidden: Boolean = this.isHidden): SqlRep[S, R] = {
    val isHidden1 = isHidden
    this.copy(isHidden = isHidden1)
  }

  def order_ext[K](implicit columnGen: R <:< Rep[K], wtImplicit: Rep[K] => ColumnOrdered[K], typeTypedK: TypedType[K]): SqlRep[S, R] = {
    val convert1: S => Rep[K] = table =>
      f(table)
    val sqlOrder1 = new SqlOrder[S] {
      override type ValType = K
      override val wt = wtImplicit
      override val convert = convert1
    }
    this.copy(sqlOrder = Option(sqlOrder1))
  }

  def orderTarget(targetName: String): SqlRep[S, R] = {
    val targetName1 = targetName
    this.copy(orderTarget = Option(targetName1))
  }

  def order: SqlRep[S, R] = ???

  def copy(proName: String = this.proName, isHidden: Boolean = this.isHidden, f: S => R = this.f,
           orderTarget: Option[String] = this.orderTarget, sqlOrder: Option[SqlOrder[S]] = this.sqlOrder): SqlRep[S, R] = {
    type R1 = R
    type T1 = T
    type G1 = G
    val proName1 = proName
    val isHidden1 = isHidden
    val f1 = f
    val shape1 = this.shape
    val valueTypeTag1 = this.valueTypeTag
    val jsonEncoder1 = this.jsonEncoder
    val orderTarget1 = orderTarget
    val sqlOrder1 = sqlOrder
    new SqlRep[S, R] {
      //override type R = R1
      override type T = T1
      override type G = G1
      override val proName = proName1
      override val isHidden = isHidden1
      override val f = f1
      override val shape = shape1
      override val valueTypeTag = valueTypeTag1
      override val jsonEncoder = jsonEncoder1
      override val orderTarget = orderTarget1
      override val sqlOrder = sqlOrder1
    }
  }
}

case class DataGen(list: () => List[SlickData], map: () => Map[String, SlickData])
case class PropertyInfo(property: String, typeName: String, isHidden: Boolean, canOrder: Boolean)
case class QueryInfo[S](wrapper: SqlWrapper[S], dataGen: List[(String, Boolean)] => DBIO[List[DataGen]]) {

  lazy val properties: List[PropertyInfo] = wrapper.properties

  def toTableData(columnName: List[(String, Boolean)])(implicit ec: ExecutionContext): DBIO[TableData] = dataGen(columnName).map(s =>
    TableData(
      properties = this.properties,
      data = s.map(t => t.map().map { case (key, value) => key -> value.toJson } )
    )
  )

}
case class TableData(properties: List[PropertyInfo], data: List[Map[String, Json]])

case class SqlWrapper[S](
  select: List[SqlRep[S, _]],
  filters: List[SqlFilter[S]] = Nil,
  orders: List[SqlOrder[S]] = Nil
) {

  def where_ext[R <: Rep[_] : CanBeQueryCondition](f: S => R): SqlWrapper[S] = {
    val filter1 = new SqlFilter[S] {
      override type ResultType = R
      override val wt = implicitly[CanBeQueryCondition[ResultType]]
      override val convert = f
    }
    this.copy(filters = filter1 :: this.filters)
  }

  def where[R <: Rep[_] : CanBeQueryCondition](f: R): SqlWrapper[S] = ???

  def order_by_ext[K : TypedType](f: S => Rep[K])(implicit wtImplicit: Rep[K] => ColumnOrdered[K]): SqlWrapper[S] = {
    val order1 = new SqlOrder[S] {
      override type ValType = K
      override val wt = wtImplicit
      override val convert = f
    }
    this.copy(orders = order1 :: this.orders)
  }

  def order_by[K : TypedType](f: Rep[K])/*(implicit wtImplicit: Rep[K] => ColumnOrdered[K])*/: SqlWrapper[S] = ???

  lazy val repGens = {
    select match {
      case head :: tail =>
        tail.foldLeft(SelectRep.head(head))((repGen, eachSelect) => {
          repGen.append(eachSelect)
        })
      case _ =>
        throw new Exception("喵了个咪")
    }
  }

  lazy val properties = select.map(s => PropertyInfo(s.proName, s.valueTypeTag.tpe.toString, s.isHidden, orderMap.exists(_._1 == s.proName)))
  //获取列名和排序方案的 Map
  lazy val orderMap: Map[String, SqlOrder[S]] = {
    //不考虑 targetName 的基本 map
    val strSqlOrderMap: Map[String, SqlOrder[S]] = {
      select.collect {
        case s if s.sqlOrder.isDefined =>
          s.proName -> s.sqlOrder.get
      }
    }.toMap
    //对 targetName 不为空的列进行转化
    select.collect {
      case s if s.orderTarget.isDefined =>
        s.proName -> s.orderTarget.get
    }.foldLeft(strSqlOrderMap) { case (eachMap, (eachPro, eachTarget)) => {
      val plusItem = eachPro -> eachMap.get(eachTarget).getOrElse(throw new Exception("targetName: $eachTarget 对应的列没有被排序"))
      eachMap + plusItem
    } }
  }

  def queryResult(query: Query[S, _, Seq])
    (implicit ec: ExecutionContext, ev: Query[_, repGens.ValType, Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[repGens.ValType], repGens.ValType]): QueryInfo[S] = {
    val dataFun = (orderColumns: List[(String, Boolean)]) => {
      val filterQuery = filters.foldLeft(query)((fQuery, eachFilter) => {
        fQuery.filter(eachFilter.convert)(eachFilter.wt)
      })
      val codeSortQuery = orders.foldLeft(filterQuery)((fQuery, eachOrder) => {
        fQuery.sortBy(table1 => eachOrder.wt(eachOrder.convert(table1)))
      })
      val paramSortQuery = orderColumns.foldLeft(codeSortQuery) { case (cQuery, (eachOrderName, eachIsDesc)) => {
        orderMap.get(eachOrderName) match {
          case Some(sqlOrder) =>
            cQuery.sortBy(table1 => {
              val orderedColumn = sqlOrder.wt(sqlOrder.convert(table1))
              if (eachIsDesc)
                orderedColumn.desc
              else
                orderedColumn.asc
            })
          case _ => cQuery
        }
      } }
      paramSortQuery.map(repGens.repGen(_))(repGens.shape).result.map(s => s.toList.map(t => {
        val listPre = () => repGens.listGen(t)
        val mapPre = () => repGens.mapGen(t)
        DataGen(list = listPre, map = mapPre)
      }))
    }
    QueryInfo(wrapper = this, dataGen = dataFun)
  }

}

object select {

  def apply[S](columns: SqlRep[S, _]*) = {
    SqlWrapper(
      select = columns.toList
    )
  }

}

trait SelectRep[S] {
  type ColType
  type ValType
  type TargetColType
  val shape: Shape[_ <: FlatShapeLevel, ColType, ValType, TargetColType]
  val listGen: ValType => List[SlickData]
  val mapGen: ValType => Map[String, SlickData]
  val repGen: S => ColType
  //val repMap: Map[String, SqlRep[S]]

  def append[RT](baseRep: SqlRep[S, RT]): SelectRep[S] = {
    type ColType1 = (ColType, RT)
    type ValType1 = (ValType, baseRep.T)
    type TargetColType1 = (TargetColType, baseRep.G)
    val shape1 = new TupleShape[FlatShapeLevel, ColType1, ValType1, TargetColType1](shape, baseRep.shape)
    val listGen1: ValType1 => List[SlickData] = (newValue) => {
      val baseList = listGen(newValue._1)
      val appendValue = newValue._2
      val appendSlickData = new SlickData {
        override val property = baseRep.proName
        override type DataType = baseRep.T
        override val data = appendValue
        override val jsonEncoder = baseRep.jsonEncoder
        override val typeTag = baseRep.valueTypeTag
      }
      baseList ::: appendSlickData :: Nil
    }
    val mapGen1: ValType1 => Map[String, SlickData] = (newValue) => {
      val baseList = mapGen(newValue._1)
      val appendValue = newValue._2
      val appendSlickData = new SlickData {
        override val property = baseRep.proName
        override type DataType = baseRep.T
        override val data = appendValue
        override val jsonEncoder = baseRep.jsonEncoder
        override val typeTag = baseRep.valueTypeTag
      }
      baseList + (baseRep.proName -> appendSlickData)
    }
    val repGen1: S => ColType1 = sourceTable => {
      val initCols = repGen(sourceTable)
      val newCol = baseRep.f(sourceTable)
      initCols -> newCol
    }
    //val repMap1 = repMap + (baseRep.proName -> baseRep)

    new SelectRep[S] {
      override type ColType = ColType1
      override type ValType = ValType1
      override type TargetColType = TargetColType1
      override val shape = shape1
      override val listGen = listGen1
      override val mapGen = mapGen1
      override val repGen = repGen1
      //override val repMap = repMap1
    }
  }
}

object SelectRep {

  def head[S, RT](baseRep: SqlRep[S, RT]): SelectRep[S] = {
    new SelectRep[S] {
      override type ColType = Tuple1[RT]
      override type ValType = Tuple1[baseRep.T]
      override type TargetColType = Tuple1[baseRep.G]
      override val shape = new TupleShape[FlatShapeLevel, Tuple1[RT], Tuple1[baseRep.T], Tuple1[baseRep.G]](baseRep.shape)
      override val listGen = (baseVal: ValType) => {
        val initValue = new SlickData {
          override val property = baseRep.proName
          override type DataType = baseRep.T
          override val data = baseVal._1
          override val jsonEncoder = baseRep.jsonEncoder
          override val typeTag = baseRep.valueTypeTag
        }
        initValue :: Nil
      }
      override val mapGen = (baseVal: ValType) => {
        val initValue = new SlickData {
          override val property = baseRep.proName
          override type DataType = baseRep.T
          override val data = baseVal._1
          override val jsonEncoder = baseRep.jsonEncoder
          override val typeTag = baseRep.valueTypeTag
        }
        Map(baseRep.proName -> initValue)
      }
      override val repGen = (baseTable: S) => {
        Tuple1(baseRep.f(baseTable))
      }
      //override val repMap = Map(baseRep.proName -> baseRep)
    }
  }

}