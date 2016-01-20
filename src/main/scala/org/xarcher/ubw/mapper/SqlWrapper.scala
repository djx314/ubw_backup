package org.xarcher.ubw.mapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import scala.reflect.runtime.universe._
import slick.dbio._
import slick.driver.{JdbcProfile, JdbcActionComponent}
import slick.lifted._
import scala.language.existentials

case class SqlWrapper[S](
  select: List[SqlRep[S, _, _, _]],
  query: Query[S, _, Seq]
  //filters: List[SqlFilter[S]] = Nil,
  //orders: List[SqlOrder[S]] = Nil,
  //groupBy: Option[SqlGroupBy[S]] = None
) {

  /*def where_ext[R <: Rep[_] : CanBeQueryCondition](f: S => R): this.type = {
    val filter1 = new SqlFilter[S] {
      override type ResultType = R
      override val wt = implicitly[CanBeQueryCondition[ResultType]]
      override val convert = f
    }
    this.copy(filters = filter1 :: this.filters).asInstanceOf[this.type]
  }

  def where_if_ext[R <: Rep[_] : CanBeQueryCondition](need: Boolean)(f: S => R): this.type = {
    if (need) {
      val filter1 = new SqlFilter[S] {
        override type ResultType = R
        override val wt = implicitly[CanBeQueryCondition[ResultType]]
        override val convert = f
      }
      this.copy(filters = filter1 :: this.filters).asInstanceOf[this.type]
    } else
      this
  }

  def where[R <: Rep[_] : CanBeQueryCondition](f: R): this.type = ???

  def where_if[R <: Rep[_] : CanBeQueryCondition](need: Boolean)(f: R): this.type = ???

  def order_by_ext[K](f: S => K)(implicit wtImplicit: K => Ordered): this.type = {
    val order1 = new SqlOrder[S] {
      override type RepType = K
      override val wt = wtImplicit
      override val convert = f
    }
    this.copy(orders = order1 :: this.orders).asInstanceOf[this.type]
  }

  def order_by_if_ext[K](need: Boolean)(f: S => K)(implicit wtImplicit: K => Ordered): this.type = {
    if (need) {
      val order1 = new SqlOrder[S] {
        override type RepType = K
        override val wt = wtImplicit
        override val convert = f
      }
      this.copy(orders = order1 :: this.orders).asInstanceOf[this.type]
    } else
      this
  }

  def order_by[K](f: K)(implicit wtImplicit: K => Ordered): this.type = ???

  def order_by_if[K](need: Boolean)(f: K)(implicit wtImplicit: K => Ordered): this.type = ???*/

  /*def group_by_ext[K1, T1, G1](f: S => K1)(implicit kshape1: Shape[_ <: FlatShapeLevel, K1, T1, G1], vshape1: Shape[_ <: FlatShapeLevel, S, _, S]): this.type = {
    val groupBy1 = new SqlGroupBy[S] {
      override type RepType = K1
      override type T = T1
      override type G = G1

      override val convert = f

      override val kshape = kshape1
      override val vshape = vshape1

    }
    this.copy(groupBy = Option(groupBy1)).asInstanceOf[this.type]
  }

  def group_by[K1, T1, G1](f: K1)(implicit kshape1: Shape[_ <: FlatShapeLevel, K1, T1, G1]): this.type = ???*/

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
      case s if s.orderTargetName.isDefined =>
        s.proName -> s.orderTargetName.get
    }.foldLeft(strSqlOrderMap) { case (eachMap, (eachPro, eachTarget)) => {
      val plusItem = eachPro -> eachMap.get(eachTarget).getOrElse(throw new Exception("targetName: $eachTarget 对应的列没有被排序"))
      eachMap + plusItem
    } }
  }

  def result/*(query: Query[S, _, Seq])*/
    (implicit
     ec: ExecutionContext, ev: Query[_, repGens.ValType, Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[repGens.ValType], repGens.ValType],
     av: Rep[Int] => JdbcProfile#QueryActionExtensionMethods[Int, NoStream]
    ): QueryInfo = {
    val dataFun = (limit: SlickParam) => {
      /*val filterQuery = filters.foldLeft(query)((fQuery, eachFilter) => {
        fQuery.filter(eachFilter.convert)(eachFilter.wt)
      })

      val codeSortQuery = orders.foldLeft(filterQuery)((fQuery, eachOrder) => {
        fQuery.sortBy(table1 => eachOrder.wt(eachOrder.convert(table1)))
      })*/

      val sortQuery = limit.orders.foldLeft(query) { case (eachQuery, ColumnOrder(eachOrderName, eachIsDesc)) =>
        orderMap.get(eachOrderName) match {
          case Some(orderRep) =>
            eachQuery.sortBy(s => {
              val colOrder = orderRep.wt(orderRep.convert(s))
              if (eachIsDesc)
                colOrder.desc.nullsLast
              else
                colOrder.asc.nullsLast
            })
          case _ =>
            eachQuery
        }
      }
      val baseQuery = {
        /*val resultQuery = codeSortQuery*/sortQuery.map(repGens.repGen(_))(repGens.shape)
        /*limit.orders.foldLeft(resultQuery) { case (eachQuery, ColumnOrder(eachOrderName, eachIsDesc)) =>
          orderMap.get(eachOrderName) match {
            case Some(orderRep) =>
              eachQuery.sortBy(s => {
                val colOrder = orderRep.wt(orderRep.typeConvert(s))
                if (eachIsDesc)
                  colOrder.desc.nullsLast
                else
                  colOrder.asc.nullsLast
              })
            case _ =>
              eachQuery
          }
        }*/
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
    QueryInfo(properties = this.properties, dataGen = dataFun)
  }

}
/*object select {

  def apply[S](columns: SqlRep[S, _, _, _]*): SqlWrapper[S] = {
    SqlWrapper(
      select = columns.toList
    )
  }

}*/
trait SelectRep[S] {
  type ColType
  type ValType
  type TargetColType
  val shape: Shape[_ <: FlatShapeLevel, ColType, ValType, TargetColType]
  val listGen: ValType => List[SlickData]
  val mapGen: ValType => Map[String, SlickData]
  val repGen: S => ColType
  //val orderGen: Map[String, TargetColType => ColumnOrdered[_]]

  def append[RT, G1, T1](baseRep: SqlRep[S, RT, T1, G1]): SelectRep[S] = {
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
        override val poiWriter = baseRep.poiWritter
        override val isHidden = baseRep.isHidden
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
        override val poiWriter = baseRep.poiWritter
        override val isHidden = baseRep.isHidden
      }
      baseList + (baseRep.proName -> appendSlickData)
    }
    val repGen1: S => ColType1 = sourceTable => {
      val initCols = repGen(sourceTable)
      val newCol = baseRep.f(sourceTable)
      initCols -> newCol
    }
    /*val orderGen1: Map[String, TargetColType1 => ColumnOrdered[_]] = {
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
            sqlOrder.wt(sqlOrder.convert(newCol))
        }
        oldMap + (baseRep.proName -> newConvert)
      })
      .getOrElse(oldMap)
    }*/

    new SelectRep[S] {
      override type ColType = ColType1
      override type ValType = ValType1
      override type TargetColType = TargetColType1
      override val shape = shape1
      override val listGen = listGen1
      override val mapGen = mapGen1
      override val repGen = repGen1
      //override val orderGen = orderGen1
    }
  }
}

object SelectRep {

  def head[S, RT, G1, T1](baseRep: SqlRep[S, RT, T1, G1]): SelectRep[S] = {
    new SelectRep[S] {
      override type ColType = Tuple1[RT]
      override type ValType = Tuple1[T1]
      override type TargetColType = Tuple1[G1]
      override val shape = new TupleShape[FlatShapeLevel, Tuple1[RT], Tuple1[T1/*baseRep.T*/], Tuple1[G1]](baseRep.shape)
      override val listGen = (baseVal: ValType) => {
        val initValue = new SlickData {
          override val property = baseRep.proName
          override type DataType = T1
          override val data = baseVal._1
          override val jsonEncoder = baseRep.jsonEncoder
          override val typeTag = baseRep.valueTypeTag
          override val poiWriter = baseRep.poiWritter
          override val isHidden = baseRep.isHidden
        }
        initValue :: Nil
      }
      override val mapGen = (baseVal: ValType) => {
        val initValue = new SlickData {
          override val property = baseRep.proName
          override type DataType = T1
          override val data = baseVal._1
          override val jsonEncoder = baseRep.jsonEncoder
          override val typeTag = baseRep.valueTypeTag
          override val poiWriter = baseRep.poiWritter
          override val isHidden = baseRep.isHidden
        }
        Map(baseRep.proName -> initValue)
      }
      override val repGen = (baseTable: S) => {
        Tuple1(baseRep.f(baseTable))
      }
      /*override val orderGen = {
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
      }*/
    }
  }

}