package org.xarcher.ubw.wrapper

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import slick.dbio._
import slick.driver.JdbcActionComponent
import slick.lifted._

/**
  * Created by djx314 on 15-5-24.
  */
trait SlickData {

  val property: String
  type DataType
  val data: DataType

  override def toString = s"SlickData(property=$property,data=$data)"

}

trait SqlFilter[S] {

  type ResultType <: Rep[_]
  type TableType = S
  val wt: CanBeQueryCondition[ResultType]
  val convert: TableType => ResultType

}

trait SqlOrder[S] {

  type ResultType
  type TableType = S
  val wt: ResultType => Ordered
  val convert: TableType => ResultType

}

trait SqlRep[S] {
  type R
  type T
  type G
  val proName: String
  val f: S => R
  val shape: Shape[_ <: FlatShapeLevel, R, T, G]
}

case class SqlWrapper[S](
  select: List[SqlRep[S]],
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

  def order_by_ext[R](f: S => R)(implicit wtImplicit: R => Ordered): SqlWrapper[S] = {
    val order1 = new SqlOrder[S] {
      override type ResultType = R
      override val wt = wtImplicit
      override val convert = f
    }
    this.copy(orders = order1 :: this.orders)
  }

  def order_by[R](f: R)(implicit wtImplicit: R => Ordered): SqlWrapper[S] = ???

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

  case class DataGen(list: () => List[SlickData], map: () => Map[String, SlickData])

  def queryResult[E[_]](query: Query[S, _, Seq])
    (implicit ec: ExecutionContext, ev: Query[_, repGens.ValType, Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[repGens.ValType], repGens.ValType]): DBIO[Seq[DataGen]] = {
    val filterQuery = filters.foldLeft(query)((fQuery, eachFilter) => {
      fQuery.filter(eachFilter.convert)(eachFilter.wt)
    })
    val sortQuery = orders.foldLeft(filterQuery)((fQuery, eachOrder) => {
      fQuery.sortBy(table1 => eachOrder.wt(eachOrder.convert(table1)))
    })
    sortQuery.map(repGens.repGen(_))(repGens.shape).result.map(s => s.toList.map(t => {
      val listPre = () => repGens.listGen(t)
      val mapPre = () => repGens.mapGen(t)
      DataGen(list = listPre, map = mapPre)
    }))
  }

}

object select {

  def apply[S](columns: SqlRep[S]*) = {
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

  def append(baseRep: SqlRep[S]): SelectRep[S] = {
    type ColType1 = (ColType, baseRep.R)
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
      }
      baseList ::: appendSlickData :: Nil
    }
    val mapGen1: ValType1 => Map[String, SlickData] = (newValue) => {
      val baseList = mapGen(newValue._1)
      val appendValue = newValue._2
      val appendSlickData = new SlickData {
        val property = baseRep.proName
        type DataType = baseRep.T
        val data = appendValue
      }
      baseList + (baseRep.proName -> appendSlickData)
    }
    val repGen1: S => ColType1 = sourceTable => {
      val initCols = repGen(sourceTable)
      val newCol = baseRep.f(sourceTable)
      initCols -> newCol
    }

    new SelectRep[S] {
      override type ColType = ColType1
      override type ValType = ValType1
      override type TargetColType = TargetColType1
      override val shape = shape1
      override val listGen = listGen1
      override val mapGen = mapGen1
      override val repGen = repGen1
    }
  }
}

object SelectRep {

  def head[S](baseRep: SqlRep[S]): SelectRep[S] = {
    new SelectRep[S] {
      override type ColType = Tuple1[baseRep.R]
      override type ValType = Tuple1[baseRep.T]
      override type TargetColType = Tuple1[baseRep.G]
      override val shape = new TupleShape[FlatShapeLevel, Tuple1[baseRep.R], Tuple1[baseRep.T], Tuple1[baseRep.G]](baseRep.shape)
      override val listGen = (baseVal: ValType) => {
        val initValue = new SlickData {
          override val property = baseRep.proName
          override type DataType = baseRep.T
          override val data = baseVal._1
        }
        initValue :: Nil
      }
      override val mapGen = (baseVal: ValType) => {
        val initValue = new SlickData {
          override val property = baseRep.proName
          override type DataType = baseRep.T
          override val data = baseVal._1
        }
        Map(baseRep.proName -> initValue)
      }
      override val repGen = (baseTable: S) => {
        Tuple1(baseRep.f(baseTable))
      }
    }
  }

}