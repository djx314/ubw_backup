package org.xarcher.ubw.dsl

import slick.lifted.{CanBeQueryCondition, Ordered}

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
//import slick.dbio._
import slick.driver.JdbcActionComponent
import slick.driver.H2Driver.api._
//import slick.lifted._

/**
  * Created by djx314 on 15-5-24.
  */
trait SlickData {

  val property: String
  type DataType
  val data: DataType

  override def toString = s"SlickData(property=$property,data=$data)"

}

trait SqlFilter {

  type ResultType <: Rep[_]
  val wt: CanBeQueryCondition[ResultType]
  val convert: ResultType

}

trait SqlOrder {

  type ResultType
  val wt: ResultType => Ordered
  val convert: ResultType

}

trait SqlRep[S] {
  type R
  type T
  type G
  val proName: String
  val f: S => R
  val shape: Shape[_ <: FlatShapeLevel, R, T, G]
}

trait SqlMap[R, T, G] {
  val convert: R
  val shape: Shape[_ <: FlatShapeLevel, R, T, G]
}

object SqlWrapper {

  def aaaaResult[S, U, R, T, G](query: Query[S, U, Seq], wrapperGen: S => SqlWrapper[R, T, G]): Query[G, T, Seq] = {
    query.flatMap(s => {
      def createQuery = query
      val wrapper = wrapperGen(s)
      val filterQuery = wrapper.filters match {
        case head :: tail =>
          tail.foldLeft(createQuery.filter(_ => head.convert)(head.wt))((eachQuery, eachFilter) => {
            eachQuery.flatMap(_ => createQuery.filter(_ => eachFilter.convert)(eachFilter.wt))
          })
        case _ => throw new Exception("喵了个咪，空 list 作甚")
      }
      val sortQuery = wrapper.orders match {
        case head :: tail =>
          tail.foldLeft(createQuery.sortBy(_ => head.convert)(head.wt))((eachQuery, eachFilter) => {
            eachQuery.flatMap(_ => createQuery.sortBy(_ => eachFilter.convert)(eachFilter.wt))
          })
        case _ => throw new Exception("喵了个咪，空 list 作甚")
      }
      filterQuery.map(_ => wrapper.map.convert)(wrapper.map.shape)
    })
  }

  def map[R, T, G](columnsGen: R)(implicit shape1: Shape[FlatShapeLevel, R, T, G]) = {
    val map1 = new SqlMap[R, T, G] {
      override val convert = columnsGen
      override val shape = shape1
    }
    SqlWrapper[R, T, G](
      map = map1
    )
  }

}

case class SqlWrapper[R, T, G](
  //select: List[SqlRep[S]],
  map: SqlMap[R, T, G],
  filters: List[SqlFilter] = Nil,
  orders: List[SqlOrder] = Nil
) {

  /*def where_ext[R <: Rep[_] : CanBeQueryCondition](f: S => R): SqlWrapper[S] = {
    val filter1 = new SqlFilter[S] {
      override type ResultType = R
      override val wt = implicitly[CanBeQueryCondition[ResultType]]
      override val convert = f
    }
    this.copy(filters = filter1 :: this.filters)
  }*/

  def where[A <: Rep[_] : CanBeQueryCondition](f: A): SqlWrapper[R, T, G] = {
    val filter1 = new SqlFilter {
      override type ResultType = A
      override val wt = implicitly[CanBeQueryCondition[A]]
      override val convert = f
    }
    this.copy(filters = filter1 :: this.filters)
  }

  /*def order_by_ext[R](f: S => R)(implicit wtImplicit: R => Ordered): SqlWrapper[S] = {
    val order1 = new SqlOrder[S] {
      override type ResultType = R
      override val wt = wtImplicit
      override val convert = f
    }
    this.copy(orders = order1 :: this.orders)
  }*/

  def order_by[A](f: A)(implicit wtImplicit: A => Ordered): SqlWrapper[R, T, G] = {
    val order1 = new SqlOrder {
      override type ResultType = A
      override val wt = implicitly[A => Ordered]
      override val convert = f
    }
    this.copy(orders = order1 :: this.orders)
  }

  /*lazy val repGens = {
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
  }*/

}

object select {

  /*def apply[S](columns: SqlRep[S]*) = {
    SqlWrapper(
      select = columns.toList
    )
  }*/

}

/*trait SelectRep[S] {
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

}*/