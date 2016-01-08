package org.xarcher.ubw.wrapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._
import slick.ast.TypedType

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import slick.dbio._
import slick.driver.{JdbcProfile, JdbcActionComponent}
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

trait SqlRepOrder[S] {

  type ValType
  type TableType = S
  val wt: Rep[ValType] => ColumnOrdered[ValType]
  val convert: TableType => Rep[ValType]

}

trait SqlOrder[S] {

  type RepType
  type TableType = S
  val wt: RepType => Ordered
  val convert: TableType => RepType

}

trait SqlRep[S, R] {
  //type +RepType = R
  type T
  type G
  val proName: String
  val isHidden: Boolean
  val isDefaultDesc: Boolean
  val f: S => R
  val shape: Shape[_ <: FlatShapeLevel, R, T, G]
  val valueTypeTag: WeakTypeTag[T]
  val jsonEncoder: Encoder[T]
  /**
    * 如果同时拥有 orderTarget 和 ordereImplicit，以 orderTarget 为先
    */
  val orderTarget: Option[String] = None
  val sqlOrder: Option[SqlRepOrder[S]] = None

  def hidden(isHidden: Boolean = this.isHidden): SqlRep[S, R] = {
    val isHidden1 = isHidden
    this.copy(isHidden = isHidden1)
  }

  def order[K](isDefaultDesc: Boolean)(implicit columnGen: R <:< Rep[K], wtImplicit: Rep[K] => ColumnOrdered[K], typeTypedK: TypedType[K]): SqlRep[S, R] = {
    val convert1: S => Rep[K] = table =>
      f(table)
    val isDefaultDesc1 = isDefaultDesc
    val sqlOrder1 = new SqlRepOrder[S] {
      override type ValType = K
      override val wt = wtImplicit
      override val convert = convert1
    }
    this.copy(sqlOrder = Option(sqlOrder1), isDefaultDesc = isDefaultDesc1)
  }

  def orderTarget(targetName: String, isDefaultDesc: Boolean): SqlRep[S, R] = {
    val targetName1 = targetName
    val isDefaultDesc1 = isDefaultDesc
    this.copy(orderTarget = Option(targetName1), isDefaultDesc = isDefaultDesc1)
  }

  //def order11111111(isDefaultDesc: Boolean): SqlRep[S, R] = ???

  def copy(proName: String = this.proName, isHidden: Boolean = this.isHidden, isDefaultDesc: Boolean = this.isDefaultDesc, f: S => R = this.f,
           orderTarget: Option[String] = this.orderTarget, sqlOrder: Option[SqlRepOrder[S]] = this.sqlOrder): SqlRep[S, R] = {
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
    val orderTarget1 = orderTarget
    val sqlOrder1 = sqlOrder
    new SqlRep[S, R] {
      //override type R = R1
      override type T = T1
      override type G = G1
      override val proName = proName1
      override val isHidden = isHidden1
      override val isDefaultDesc = isDefaultDesc1
      override val f = f1
      override val shape = shape1
      override val valueTypeTag = valueTypeTag1
      override val jsonEncoder = jsonEncoder1
      override val orderTarget = orderTarget1
      override val sqlOrder = sqlOrder1
    }
  }
}

case class SlickRange(drop: Long, take: Long)
case class SlickPage(pageIndex: Long, pageSize: Long)
case class ColumnOrder(columnName: String, isDesc: Boolean)

case class SlickParam(orders: List[ColumnOrder] = Nil, range: Option[SlickRange] = None, page: Option[SlickPage] = None)

case class DataGen(list: () => List[SlickData], map: () => Map[String, SlickData])
case class ResultGen(data: List[DataGen], sum: Long)
case class PropertyInfo(property: String, typeName: String, isHidden: Boolean, canOrder: Boolean, isDefaultDesc: Boolean)
case class QueryInfo[S](wrapper: SqlWrapper[S], dataGen: SlickParam => DBIO[ResultGen]) {

  lazy val properties: List[PropertyInfo] = wrapper.properties

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

  def where_if_ext[R <: Rep[_] : CanBeQueryCondition](need: Boolean)(f: S => R): SqlWrapper[S] = {
    if (need) {
      val filter1 = new SqlFilter[S] {
        override type ResultType = R
        override val wt = implicitly[CanBeQueryCondition[ResultType]]
        override val convert = f
      }
      this.copy(filters = filter1 :: this.filters)
    } else
      this
  }

  def where[R <: Rep[_] : CanBeQueryCondition](f: R): SqlWrapper[S] = ???

  def where_if[R <: Rep[_] : CanBeQueryCondition](need: Boolean)(f: R): SqlWrapper[S] = ???

  def order_by_ext[K](f: S => K)(implicit wtImplicit: K => Ordered): SqlWrapper[S] = {
    val order1 = new SqlOrder[S] {
      override type RepType = K
      override val wt = wtImplicit
      override val convert = f
    }
    this.copy(orders = order1 :: this.orders)
  }

  def order_by_if_ext[K](need: Boolean)(f: S => K)(implicit wtImplicit: K => Ordered): SqlWrapper[S] = {
    if (need) {
      val order1 = new SqlOrder[S] {
        override type RepType = K
        override val wt = wtImplicit
        override val convert = f
      }
      this.copy(orders = order1 :: this.orders)
    } else
      this
  }

  def order_by[K](f: K)(implicit wtImplicit: K => Ordered): SqlWrapper[S] = ???

  def order_by_if[K](need: Boolean)(f: K)(implicit wtImplicit: K => Ordered): SqlWrapper[S] = ???

  lazy val repGens = {
    select.filter(s => ! s.isHidden) match {
      case head :: tail =>
        tail.foldLeft(SelectRep.head(head))((repGen, eachSelect) => {
          repGen.append(eachSelect)
        })
      case _ =>
        throw new Exception("喵了个咪")
    }
  }

  lazy val properties = select.map(s => PropertyInfo(s.proName, s.valueTypeTag.tpe.toString, s.isHidden, orderMap.exists(_._1 == s.proName), s.isDefaultDesc))
  //获取列名和排序方案的 Map
  lazy val orderMap: Map[String, SqlRepOrder[S]] = {
    //不考虑 targetName 的基本 map
    val strSqlOrderMap: Map[String, SqlRepOrder[S]] = {
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
    (implicit
     ec: ExecutionContext, ev: Query[_, repGens.ValType, Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[repGens.ValType], repGens.ValType],
     av: Rep[Int] => JdbcProfile#QueryActionExtensionMethods[Int, NoStream]
    ): QueryInfo[S] = {
    val dataFun = (limit: SlickParam) => {
      val filterQuery = filters.foldLeft(query)((fQuery, eachFilter) => {
        fQuery.filter(eachFilter.convert)(eachFilter.wt)
      })
      val codeSortQuery = orders.foldLeft(filterQuery)((fQuery, eachOrder) => {
        fQuery.sortBy(table1 => eachOrder.wt(eachOrder.convert(table1)))
      })
      val paramSortQuery = limit.orders.foldLeft(codeSortQuery) { case (cQuery, ColumnOrder(eachOrderName, eachIsDesc)) => {
        orderMap.get(eachOrderName) match {
          case Some(sqlOrder) =>
            cQuery.sortBy(table1 => {
              val orderedColumn = sqlOrder.wt(sqlOrder.convert(table1))
              if (eachIsDesc)
                orderedColumn.desc.nullsLast
              else
                orderedColumn.asc.nullsLast
            })
          case _ => cQuery
        }
      } }

      val baseQuery = paramSortQuery.map(repGens.repGen(_))(repGens.shape)

      limit match {
        case SlickParam(_, Some(SlickRange(drop1, take1)), Some(SlickPage(pageIndex1, pageSize1))) =>

          println(limit.toString * 100)

          val startCount = Math.max(0, drop1)
          val pageIndex = Math.max(0, pageIndex1)
          val pageSize = Math.max(0, pageSize1)

          val dropQuery = baseQuery.drop(startCount)

          (for {
            sum <- dropQuery.size.result
          } yield {
            val pageStart = startCount + pageIndex * pageSize
            val pageEnd = pageStart + pageSize
            val endCount = Math.min(take1, startCount + sum)
            val autalStart = Math.max(pageStart, startCount)
            val autalEnd = Math.min(pageEnd, endCount)
            val autalLimit = Math.max(0, autalEnd - autalStart)

            val limitQuery = dropQuery.drop(pageIndex * pageSize).take(autalLimit)

            limitQuery.result.map(s => {
              val dataGen = s.toList.map(t => {
                val listPre = () => repGens.listGen(t)
                val mapPre = () => repGens.mapGen(t)
                DataGen(list = listPre, map = mapPre)
              })
              ResultGen(dataGen, sum)
            })
          })
          .flatMap(s => s)
        case SlickParam(_, Some(SlickRange(drop, take)), None) =>
          val dropQuery = baseQuery.drop(drop)
          val takeQuery = dropQuery.take(take)

          takeQuery.result.map(s => {
            val dataGen = s.toList.map(t => {
              val listPre = () => repGens.listGen(t)
              val mapPre = () => repGens.mapGen(t)
              DataGen(list = listPre, map = mapPre)
            })
            ResultGen(dataGen, s.size)
          })
        case SlickParam(_, None, Some(SlickPage(pageIndex, pageSize))) =>
          val dropQuery = baseQuery.drop(pageIndex * pageSize)
          val takeQuery = dropQuery.take(pageSize)

          for {
            sum <- baseQuery.size.result
            s <- takeQuery.result
          } yield {
            val dataGen = s.toList.map(t => {
              val listPre = () => repGens.listGen(t)
              val mapPre = () => repGens.mapGen(t)
              DataGen(list = listPre, map = mapPre)
            })
            ResultGen(dataGen, sum)
          }
        case _ =>
          baseQuery.result.map(s => {
            val dataGen = s.toList.map(t => {
              val listPre = () => repGens.listGen(t)
              val mapPre = () => repGens.mapGen(t)
              DataGen(list = listPre, map = mapPre)
            })
            ResultGen(dataGen, s.size)
          })
      }

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
    /*if (baseRep.isHidden) {
      this
    } else {*/
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
    /*}*/
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