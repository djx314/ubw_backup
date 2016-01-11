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

  def asQ[H, P, N](implicit mshape: Shape[_ <: FlatShapeLevel, R, T, N], mikuShape: Shape[_ <: FlatShapeLevel, N, P, H], jsonEncoder1: Encoder[P], valueTypeTag1: WeakTypeTag[P]) = (convert: Query[N, T, Seq] => H) => {
    /*val middleQuery: Query[S, T, Seq] => Query[G, T, Seq] = query => {
      query.map(f(_))(shape)
    }*/
    type S1 = S
    type R1 = R
    type G1 = G
    type T1 = T

    val f1 = this.f

    val proName1 = this.proName
    val isHidden1 = this.isHidden
    val isDefaultDesc1 = this.isDefaultDesc
    val orderTargetName1 = this.orderTargetName
    val sqlOrder1 = this.sqlOrder
    //val shape1 = this.shape

    new SqlQRep[S1, N, H, P] {
      override type U = R1
      override type B = T1
      //override type T = P
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

trait SqlQRep[S, R, G, T] extends SqlRepBase[S, R, G, T] {

  type U
  type B

  val f: S => U
  val ushape: Shape[_ <: FlatShapeLevel, U, B, R]
  //val shape: Shape[_ <: FlatShapeLevel, R, T, G]
  val e: Query[R, B, Seq] => G

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
    f: S => U = this.f, e: Query[R, B, Seq] => G = this.e,
    orderTargetName: Option[String] = this.orderTargetName, sqlOrder: Option[SqlRepOrder[G]] = this.sqlOrder): this.type = {
    type U1 = U
    type B1 = B

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
    new SqlQRep[S, R, G, T1] {
      override type U = U1
      override type B = B1

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
  select: List[SqlRep[S, _, _, _]],
  filters: List[SqlFilter[S]] = Nil,
  orders: List[SqlOrder[S]] = Nil,
  groupBy: Option[SqlGroupBy[S]] = None
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

  def group_by_ext[K1, T1, G1](f: S => K1)(implicit kshape1: Shape[_ <: FlatShapeLevel, K1, T1, G1], vshape1: Shape[_ <: FlatShapeLevel, S, _, S]): SqlWrapper[S] = {
    val groupBy1 = new SqlGroupBy[S] {
      override type RepType = K1
      override type T = T1
      override type G = G1

      override val convert = f

      override val kshape = kshape1
      override val vshape = vshape1

    }
    this.copy(groupBy = Option(groupBy1))
  }

  def group_by[K1, T1, G1](f: K1)(implicit kshape1: Shape[_ <: FlatShapeLevel, K1, T1, G1]): SqlWrapper[S] = ???

  lazy val repGens = {
    //暂时隐藏 filter，估计以后也不会再开了
    select/*.filter(s => ! s.isHidden)*/ match {
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
  lazy val orderMap: Map[String, repGens.TargetColType => ColumnOrdered[_]] = {
    select.foldLeft(repGens.orderGen)((orderGen, eachSelect) => {
      eachSelect.orderTargetName match {
        case Some(targetName) =>
          val plusItem = eachSelect.proName -> orderGen.get(targetName).getOrElse(throw new Exception(s"targetName: $targetName 对应的列没有被排序"))
          orderGen + plusItem
        case _ =>
          orderGen
      }
    })
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

      val baseQuery = groupBy match {
        case Some(eachGroupBy) =>
          codeSortQuery.groupBy(eachGroupBy.convert)(eachGroupBy.kshape, eachGroupBy.vshape).flatMap { case (key, valueQuery) => valueQuery.map(repGens.repGen(_))(repGens.shape) }
        case _ =>
          val resultQuery = codeSortQuery.map(repGens.repGen(_))(repGens.shape)
          limit.orders.foldLeft(resultQuery) { case (eachQuery, ColumnOrder(eachOrderName, eachIsDesc)) =>
            orderMap.get(eachOrderName) match {
              case Some(convert) =>
                eachQuery.sortBy(s => {
                  val colOrder = convert(s)
                  if (eachIsDesc)
                    colOrder.desc.nullsLast
                  else
                    colOrder.asc.nullsLast
                })
              case _ =>
                eachQuery
            }
          }
      }

      limit match {
        case SlickParam(_, Some(SlickRange(drop1, take1)), Some(SlickPage(pageIndex1, pageSize1))) =>
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

  def apply[S](columns: SqlRep[S, _, _, _]*) = {
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
  val orderGen: Map[String, TargetColType => ColumnOrdered[_]]

  def append[RT, G1, T1](baseRep: SqlRep[S, RT, G1, T1]): SelectRep[S] = {
    type ColType1 = (ColType, RT)
    type ValType1 = (ValType, T1/*baseRep.T*/)
    type TargetColType1 = (TargetColType, G1)
    val shape1 = new TupleShape[FlatShapeLevel, ColType1, ValType1, TargetColType1](shape, baseRep.shape)
    val listGen1: ValType1 => List[SlickData] = (newValue) => {
      val baseList = listGen(newValue._1)
      val appendValue = newValue._2
      val appendSlickData = new SlickData {
        override val property = baseRep.proName
        override type DataType = T1/*baseRep.T*/
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
        override type DataType = T1/*baseRep.T*/
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
    val orderGen1: Map[String, TargetColType1 => ColumnOrdered[_]] = {
      val oldMap = orderGen.map { case (key, convert) => {
        val convert1: TargetColType1 => ColumnOrdered[_] = {
          case (currentTargetCol, newCol) =>
            convert(currentTargetCol)
        }
        key -> convert1
      } }
      (for {
        sqlOrder <- baseRep.sqlOrder
      } yield {
        val newConvert: TargetColType1 => ColumnOrdered[_] = {
          case (currentTargetCol, newCol) =>
            sqlOrder.wt(sqlOrder.typeConvert(newCol))
        }
        oldMap + (baseRep.proName -> newConvert)
      })
      .getOrElse(oldMap)
    }

    new SelectRep[S] {
      override type ColType = ColType1
      override type ValType = ValType1
      override type TargetColType = TargetColType1
      override val shape = shape1
      override val listGen = listGen1
      override val mapGen = mapGen1
      override val repGen = repGen1
      override val orderGen = orderGen1
    }
  }
}

object SelectRep {

  def head[S, RT, G1, T1](baseRep: SqlRep[S, RT, G1, T1]): SelectRep[S] = {
    new SelectRep[S] {
      override type ColType = Tuple1[RT]
      override type ValType = Tuple1[T1/*baseRep.T*/]
      override type TargetColType = Tuple1[G1]
      override val shape = new TupleShape[FlatShapeLevel, Tuple1[RT], Tuple1[T1/*baseRep.T*/], Tuple1[G1]](baseRep.shape)
      override val listGen = (baseVal: ValType) => {
        val initValue = new SlickData {
          override val property = baseRep.proName
          override type DataType = T1/*baseRep.T*/
          override val data = baseVal._1
          override val jsonEncoder = baseRep.jsonEncoder
          override val typeTag = baseRep.valueTypeTag
        }
        initValue :: Nil
      }
      override val mapGen = (baseVal: ValType) => {
        val initValue = new SlickData {
          override val property = baseRep.proName
          override type DataType = T1/*baseRep.T*/
          override val data = baseVal._1
          override val jsonEncoder = baseRep.jsonEncoder
          override val typeTag = baseRep.valueTypeTag
        }
        Map(baseRep.proName -> initValue)
      }
      override val repGen = (baseTable: S) => {
        Tuple1(baseRep.f(baseTable))
      }
      override val orderGen = {
        (for {
          sqlOrder <- baseRep.sqlOrder
        } yield {
          val newConvert: TargetColType => ColumnOrdered[_] = {
            case Tuple1(initCol) =>
              sqlOrder.wt(sqlOrder.typeConvert(initCol))
          }
          Map(baseRep.proName -> newConvert)
        })
        .getOrElse(Map.empty[String, TargetColType => ColumnOrdered[_]])
      }
    }
  }

}