package net.scalax.ubw.mapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._

import slick.ast.{Bind, Ref, AnonSymbol}
import slick.lifted._

class QueryToUQueryExtensionMethods[E, U](val queryToExt: Query[E, U, Seq]) {

  def uFlatMap[F, T](f: E => QueryToUQueryExtensionMethods[F, T])
  : QueryToUQueryExtensionMethods[F, T] = {
    new QueryToUQueryExtensionMethods[F, T](queryToExt.flatMap(s => f(s).queryToExt))
  }

  def uMap[F, G, T](f: E => F)(implicit shape: Shape[_ <: FlatShapeLevel, F, T, G]): QueryToUQueryExtensionMethods[G, T] = {
    new QueryToUQueryExtensionMethods[G, T](queryToExt.map(f)(shape))
  }

  def flatMap(f: E => UQuery)
  : UQuery = {
    val generator = new AnonSymbol
    val aliased = queryToExt.shaped.encodeRef(Ref(generator)).value
    val fv = f(aliased)
    val fvQuery = fv.query
    val query2 = new WrappingQuery[fv.E, fv.U, Seq](new Bind(generator, queryToExt.toNode, fvQuery.toNode), fvQuery.shaped)
    new UQuery {
      override type E = fv.E
      override type U = fv.U
      override val query = query2
      override val resultConvert = fv.resultConvert
      override val properties = fv.properties
      override val orderMap = fv.orderMap
    }
  }

  def map(f: E => List[SqlRep[_, _, _]]): UQuery = {
    flatMap(s => {
      val selectRep = f(s) match {
        case head :: tail =>
          tail.foldLeft(SelectRep.head(head))((eachRep, toAppend) => {
            eachRep.append(toAppend)
          })
        case _ => throw new IllegalArgumentException("不能解析 0 列的数据结果")
      }
      val query2: Query[selectRep.TargetColType, selectRep.ValType, Seq] =/*Query(()).map(_ => selectRep.rep)(selectRep.shape)*/Query(selectRep.rep)(selectRep.shape)
      val convert = (data: selectRep.ValType) => {
        DataGen(() => selectRep.listGen(data), () => selectRep.mapGen(data))
      }

      val orderMap1: Map[String, selectRep.TargetColType => ColumnOrdered[_]] = {
        selectRep.baseSqlReps.foldLeft(selectRep.orderGen)((orderGen, eachSelect) => {
          eachSelect.orderTargetName match {
            case Some(targetName) =>
              val plusItem = eachSelect.proName -> orderGen.get(targetName).getOrElse(throw new Exception(s"targetName: $targetName 对应的列没有被排序"))
              orderGen + plusItem
            case _ =>
              orderGen
          }
        })
      }
      val properties1 = selectRep.baseSqlReps.map(s => PropertyInfo(s.proName, s.valueTypeTag.tpe.toString, s.isHidden, orderMap1.exists(_._1 == s.proName), s.isDefaultDesc))

      new UQuery {
        override type E = selectRep.TargetColType
        override type U = selectRep.ValType
        override val query = query2
        override val resultConvert = convert
        override val properties = properties1
        override val orderMap = orderMap1
      }
    })
  }

  def filter[T <: Rep[_] : CanBeQueryCondition](f: E => T): QueryToUQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new QueryToUQueryExtensionMethods(queryToExt.filter(f)(cv))
  }

  def withFilter[T : CanBeQueryCondition](f: E => T): QueryToUQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new QueryToUQueryExtensionMethods(queryToExt.withFilter(f)(cv))
  }

  def filterNot[T <: Rep[_] : CanBeQueryCondition](f: E => T): QueryToUQueryExtensionMethods[E, U] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    new QueryToUQueryExtensionMethods(queryToExt.filterNot(f)(cv))
  }

  def groupBy[K, T, G, P](f: E => K)(implicit kshape: Shape[_ <: FlatShapeLevel, K, T, G], vshape: Shape[_ <: FlatShapeLevel, E, _, P]): QueryToUQueryExtensionMethods[(G, Query[P, U, Seq]), (T, Query[P, U, Seq])] = {
    val newQuery = queryToExt.groupBy(f)(kshape, vshape)
    new QueryToUQueryExtensionMethods(newQuery)
  }

}