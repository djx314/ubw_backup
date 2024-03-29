package net.scalax.ubw.mapper

import slick.dbio._
import slick.lifted._
import slick.profile.BasicProfile
import scala.concurrent.{ExecutionContext, Await}

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
    result(Nil)
  }

  def result(orderColumn: String, isDesc: Boolean = true)(
    implicit
    streamEv: Query[E, U, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[U], U],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): QueryInfo = {
    result(List(ColumnOrder(orderColumn, isDesc)))
  }

  def result(defaultOrders: List[ColumnOrder])(
    implicit
    streamEv: Query[E, U, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[U], U],
    repToDBIO: Rep[Int] => BasicProfile#QueryActionExtensionMethods[Int, NoStream],
    ec: ExecutionContext
  ): QueryInfo = {
    val result: SlickParam => DBIO[ResultGen] = slickParam => {
      val autualOrders = defaultOrders ::: slickParam.orders
      val baseQuery = {
        autualOrders.foldLeft(query) { case (eachQuery, ColumnOrder(eachOrderName, eachIsDesc)) =>
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

  import net.scalax.ubw._

  println{
    Await.result(db.run {
      (for {
        permission <- permissionTq1.ubw if permission.describe like "%%"
        cat <- catTq1.ubw if permission.id === cat.id
      } yield {
        List(permission.typeName as "sdffdsfrett", cat.id as "喵喵喵喵" order true)
      }).result.dataGen(SlickParam(orders = List(ColumnOrder("喵喵喵喵", true)), page = Option(SlickPage(2, 10))))
    }, scala.concurrent.duration.Duration.Inf).data.map(s => s.list()).mkString("\n")

    permissionTq1.ubw.uFlatMap(permission => catTq1.ubw.uMap(cat => cat.id -> permission.typeName))
  }

  Await.result(db.run((permissionTq1.schema ++ catTq1.schema).drop), scala.concurrent.duration.Duration.Inf)

  class CommonTable(tbName: String, tag: slick.driver.H2Driver.api.Tag) extends Table[Long](tag, tbName) {
    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)

    def * = id
  }

  def genTable(tbName: String): TableQuery[CommonTable] = {
    new TableQuery(cons => new CommonTable(tbName, cons))
  }

  val tq1 = genTable("tq1")

  val tq2 = genTable("tq2")

  /*println{
    Await.result(db.run {
      (tq1.schema ++ tq2.schema).create >>
      //error here beacuse schema.create only create the id column
      (for {
        t1 <- tq1 if t1.column[String]("one_wang") like "%%"
        t2 <- tq2 if t1.column[Int]("one_miao") === t2.column[Int]("two_miao")
      } yield {
        (t1.id, t1.column[String]("one_wang"), t1.column[String]("one_ou"), t2.column[String]("two_hahahahaha"))
      }).result
    }, scala.concurrent.duration.Duration.Inf)
  }*/

  import scala.language.implicitConversions

  trait Target[T]

  object Target {
    def apply[T] = {
      new Target[T] {}
    }
  }

  trait ListConvert[S, T] {
    val convert: List[S] => List[T]
  }

  implicit def initConvert[S] = new ListConvert[S, S] {
    override val convert: List[S] => List[S] = s => s
  }

  implicit def listListToListConvert[T, R](implicit hv: ListConvert[T, R]): ListConvert[List[T], R] = {
    new ListConvert[List[T], R] {
      override val convert: List[List[T]] => List[R] = s => hv.convert(s.flatten)
    }
  }

  def unwrapList[S, T](source: List[S], target: Target[T])(implicit ev: ListConvert[S, T]): List[T] = {
    ev.convert(source)
  }

  println(unwrapList(List(List(List(List(List(List(List(List("2333", "4567")))))))), Target[String]))

}