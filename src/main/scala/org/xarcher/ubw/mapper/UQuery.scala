package org.xarcher.ubw.mapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._
import org.xarcher.cpoi.WriteableCellOperationAbs
import slick.ast.{Bind, Ref, AnonSymbol}

import slick.dbio._
import slick.lifted._
import slick.profile.BasicProfile
import scala.concurrent.{ExecutionContext, Await}
import scala.language.existentials
import scala.language.higherKinds
import scala.reflect.runtime.universe._

trait UQuery {

  type E
  type U
  val query: Query[E, U, Seq]
  val resultConvert: U => DataGen

  val properties: List[PropertyInfo]
  //获取列名和排序方案的 Map
  val orderMap: Map[String, E => ColumnOrdered[_]]

  def result(
    implicit
    streamEv: Query[E, U, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[U], U],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): QueryInfo = {
    val result: SlickParam => DBIO[ResultGen] = slickParam => {
      val baseQuery = {
        slickParam.orders.foldLeft(query) { case (eachQuery, ColumnOrder(eachOrderName, eachIsDesc)) =>
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

      slickParam match {
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
                resultConvert(t)
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
              resultConvert(t)
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
              resultConvert(t)
            })
            ResultGen(dataGen, sum)
          }
        case _ =>
          baseQuery.result.map(s => {
            val dataGen = s.toList.map(t => {
              resultConvert(t)
            })
            ResultGen(dataGen, s.size)
          })
      }
    }

    QueryInfo(properties, result)
  }

}

trait UQueryHelper {

  implicit class queryToUQueryExtendsionMethodGen[E, U](query: Query[E, U, Seq]) {

    def u = new QueryToUQueryExtensionMethods[E, U](query)

  }

  class QueryToUQueryExtensionMethods[E, U](query1: Query[E, U, Seq]) {

    def flatMap(f: E => UQuery)
    : UQuery = {
      val generator = new AnonSymbol
      val aliased = query1.shaped.encodeRef(Ref(generator)).value
      val fv = f(aliased)
      val fvQuery = fv.query
      val query2 = new WrappingQuery[fv.E, fv.U, Seq](new Bind(generator, query1.toNode, fvQuery.toNode), fvQuery.shaped)
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
        val query2: Query[selectRep.TargetColType, selectRep.ValType, Seq] = Query(()).map(_ => selectRep.rep)(selectRep.shape)//Query(selectRep.rep)(selectRep.shape)
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
      new QueryToUQueryExtensionMethods(query1.filter(f)(cv))
    }

    def withFilter[T : CanBeQueryCondition](f: E => T): QueryToUQueryExtensionMethods[E, U] = {
      val cv = implicitly[CanBeQueryCondition[T]]
      new QueryToUQueryExtensionMethods(query1.withFilter(f)(cv))
    }

    def filterNot[T <: Rep[_] : CanBeQueryCondition](f: E => T): QueryToUQueryExtensionMethods[E, U] = {
      val cv = implicitly[CanBeQueryCondition[T]]
      new QueryToUQueryExtensionMethods(query1.filterNot(f)(cv))
    }

    def groupBy[K, T, G, P](f: E => K)(implicit kshape: Shape[_ <: FlatShapeLevel, K, T, G], vshape: Shape[_ <: FlatShapeLevel, E, _, P]): QueryToUQueryExtensionMethods[(G, Query[P, U, Seq]), (T, Query[P, U, Seq])] = {
      val newQuery = query1.groupBy(f)(kshape, vshape)
      new QueryToUQueryExtensionMethods(newQuery)
    }

  }


  implicit class miaolegemiRepExtensionMethod[R1](repLike: R1) {

    def as[T1: WeakTypeTag, G1](columnName: String)(implicit shape1: Shape[_ <: FlatShapeLevel, R1, T1, G1], jsonEncoder1: Encoder[T1], writeOperation: WriteableCellOperationAbs[T1]): SqlRep[R1, T1, G1] = {
      new SqlRep[R1, T1, G1] {
        override val valueTypeTag = implicitly[WeakTypeTag[T1]]
        override val proName = columnName
        override val isHidden = false
        override val isDefaultDesc = true
        override val rep = repLike
        override val shape = shape1
        override val jsonEncoder = jsonEncoder1
        override val poiWritter = writeOperation
      }
    }

  }

}

object TestCompile extends App {

  import org.h2.jdbcx.JdbcDataSource
  import io.circe._, io.circe.generic.auto._, io.circe.syntax._
  import scala.concurrent.ExecutionContext.Implicits.global

  import org.xarcher.cpoi.PoiOperations
  object poiOperations extends PoiOperations
  import poiOperations._

  import slick.driver.H2Driver.api._

  case class Permission(
    id: Option[Long] = None,
    name: String,
    typeName: Option[String] = Some("2333"),
    describe: String = ""
  )

  class PermissionTable(tag: slick.driver.H2Driver.api.Tag) extends Table[Permission](tag, "S_PERMISSION_TYPE") {
    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
    def name = column[String]("NAME")
    def typeName = column[Option[String]]("TYPE_NAME")
    def describe = column[String]("DESCRIBE")

    def * = (id.?, name, typeName, describe) <> (Permission.tupled, Permission.unapply _)
  }

  case class Cat(
    id: Option[Long] = None,
    miao: Option[Long] = None,
    wang: String = ""
  )

  class CatTable(tag: slick.driver.H2Driver.api.Tag) extends Table[Cat](tag, "S_CAT") {
    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
    def miao = column[Option[Long]]("MIAO")
    def wang = column[String]("WANG")

    def * = (id.?, miao, wang) <> (Cat.tupled, Cat.unapply _)
  }

  lazy val db = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:miaonimeia;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource)
  }

  val permissionTq1 = TableQuery[PermissionTable]
  val catTq1 = TableQuery[CatTable]

  val aa = permissionTq1 += Permission(
    name = "bb",
    typeName = Option("bb"),
    describe = "cc"
  )
  val aabb = catTq1 += Cat(
    miao = Option(2345L),
    wang = "bb"
  )
  try {
    Await.result(db.run {
      (permissionTq1.schema ++ catTq1.schema).create >>
        aa >>
        aabb
    }, scala.concurrent.duration.Duration.Inf)
  } catch {
    case e: Exception => e.printStackTrace
  }

  println{
    Await.result(db.run {
      (for {
        permission <- permissionTq1.u if permission.describe like "%%"
        cat <- catTq1.u if permission.id === cat.id
      } yield {
        List(permission.typeName as "sdffdsfrett", cat.id as "喵喵喵喵" order true)
      }).result.dataGen(SlickParam(orders = List(ColumnOrder("喵喵喵喵", true)), page = Option(SlickPage(2, 10))))
    }, scala.concurrent.duration.Duration.Inf).data.map(s => s.list()).mkString("\n")
  }

  Await.result(db.run((permissionTq1.schema ++ catTq1.schema).drop), scala.concurrent.duration.Duration.Inf)

}