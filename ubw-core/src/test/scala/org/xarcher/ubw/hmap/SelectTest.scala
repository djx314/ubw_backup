package org.xarcher.ubw.slick

import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent._
import org.scalatest.time.{Millis, Span}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds
import slick.driver.H2Driver.api._
import slick.lifted.{AbstractTable, Ordered, CanBeQueryCondition}

/**
  * Created by djx314 on 15-6-22.
  */

case class Permission(
  id: Option[Long] = None,
  name: String,
  typeName: String = "",
  describe: String = ""
)

class PermissionTable(tag: slick.driver.H2Driver.api.Tag) extends Table[Permission](tag, "S_PERMISSION_TYPE") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def name = column[String]("NAME")
  def typeName = column[String]("TYPE_NAME")
  def describe = column[String]("DESCRIBE")

  def * = (id.?, name, typeName, describe) <> (Permission.tupled, Permission.unapply _)
}

case class Cat(
  id: Option[Long] = None,
  miao: Long = 0,
  wang: String = ""
)

class CatTable(tag: slick.driver.H2Driver.api.Tag) extends Table[Cat](tag, "S_CAT") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def miao = column[Long]("MIAO")
  def wang = column[String]("WANG")

  def * = (id.?, miao, wang) <> (Cat.tupled, Cat.unapply _)
}

class SelectTest extends FlatSpec
with ScalaFutures
with Matchers
with BeforeAndAfter
with OneInstancePerTest {

  lazy val db = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:summerTest;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource)
  }

  val permissionTq1 = TableQuery[PermissionTable]
  val catTq1 = TableQuery[CatTable]

  val oneSecondTimeOut = Timeout(Span(1000L, Millis))

  before {
    val aa = permissionTq1 += Permission(
      name = "aa",
      typeName = "bb",
      describe = "cc"
    )
    val aabb = catTq1 += Cat(
      miao = 2345L,
      wang = "bb"
    )
    try {
      db.run {
        (permissionTq1.schema ++ catTq1.schema).create >>
          aa >>
          aabb
      }.futureValue(oneSecondTimeOut)
    } catch {
      case e: Exception => e.printStackTrace
    }
  }

  after {
    db.run((permissionTq1.schema ++ catTq1.schema).drop).futureValue(oneSecondTimeOut)
  }

  "Small table" should "update some colunms" in {

    class Mlgb {
      val permissionTq = permissionTq1
    }

    val mlgb = new Mlgb()

    import mlgb._

    val query = permissionTq.filter(_.name === "aa")

    println(db.run(query.result).futureValue(oneSecondTimeOut))

    SelectMacro.decodePrintln('喵了个咪)

  }

  "aa" should "bb" in {

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

    /*case class SqlFilter[S, R <: Rep[_] : CanBeQueryCondition](f: S => R) extends SqlFilterAbs {

      override val convert = f
      override type TableType = S
      override type ResultType = R
      override val wt = implicitly[CanBeQueryCondition[ResultType]]
      /*@inline def myFilter[U, A[_]](query: Query[S, U, A]): Query[S, U, A] = {
        query.filter(table1 => f(table1))(wt)
      }*/

    }*/
    /*case class SqlOrder[S, R](f: S => R)(implicit val wt: R => Ordered) {

      @inline def myOrder[U, A[_]](query: Query[S, U, A]): Query[S, U, A] = {
        query.sortBy(table1 => wt(f(table1)))
      }

    }*/
    case class SqlSelect[S, T, R, G](f: S => R)(implicit val shape: Shape[_ <: FlatShapeLevel, R, T, G])

    case class SqlWrapper[S, T, U, G](
      select: SqlSelect[S, T, U, G],
      filters: List[SqlFilter[S]] = Nil,
      orders: List[SqlOrder[S]] = Nil
    ) {

      def where[R <: Rep[_] : CanBeQueryCondition](f: S => R) = {
        val filter1 = new SqlFilter[S] {
          override type ResultType = R
          override val wt = implicitly[CanBeQueryCondition[ResultType]]
          override val convert = f
        }
        this.copy(filters = filter1 :: this.filters)
      }

      def order_by[R](f: S => R)(implicit wtImplicit: R => Ordered) = {
        val order1 = new SqlOrder[S] {
          override type ResultType = R
          override val wt = wtImplicit
          override val convert = f
        }
        this.copy(orders = order1 :: this.orders)
      }

    }

    object select {

      def apply[S, T, U](columns: S => T)(implicit shape: Shape[_ <: FlatShapeLevel, T, U, T]) = {
        val select1 = SqlSelect(columns)
        SqlWrapper(
          select = select1
        )
      }

    }

    /*def bb(table1: PermissionTable) = {
      select { (table1.name, table1.typeName) } where { table1.describe === "cc" } order_by table1.typeName
    }*/

    def bb = {
      select(
        (table1: PermissionTable) => {
          (table1.typeName, table1.name)
        }
      )
      .where (table1 => table1.describe === "cc")
      .order_by (table1 => table1.name)
    }

    case class Nmlgb()

    object aabb {
      def aabb(query: Query[PermissionTable, Permission, Seq]) = {
        val filterQuery = bb.filters.foldLeft(query)((fQuery, eachFilter) => {
          fQuery.filter(eachFilter.convert)(eachFilter.wt)
        })
        val sortQuery = bb.orders.foldLeft(filterQuery)((fQuery, eachOrder) => {
          fQuery.sortBy(table1 => eachOrder.wt(eachOrder.convert(table1)))
        })
        sortQuery.map(table1 => bb.select.f(table1))(bb.select.shape)
      }
    }

    val query = aabb.aabb(permissionTq1).result

    db.run(query).map(println).futureValue(oneSecondTimeOut)

    def cc = {
      select(
        (table1: (PermissionTable, CatTable)) => {
          (table1._1.name, table1._1.typeName, table1._2.id)
        }
      )
      .where { case (table1, table2) => table1.describe === "cc" && table2.wang === table1.typeName }
      .order_by { case (table1, table2) => table2.wang }
    }

    object ccdd {
      def aabb(permissionTq: Query[PermissionTable, Permission, Seq], catTq: Query[CatTable, Cat, Seq]) = {
        val tupleQuery = for {
          permission <- permissionTq
          cat <- catTq
        } yield
          permission -> cat

        val filterQuery = cc.filters.foldLeft(tupleQuery) { (eachTQuery, eachFilter) => {
          eachTQuery.filter(eachFilter.convert)(eachFilter.wt)
        } }
        val sortQuery = cc.orders.foldLeft(filterQuery) { (eachTQuery, eachOrder) => {
          eachTQuery.sortBy(table1 => eachOrder.wt(eachOrder.convert(table1)))
        } }
        sortQuery.map(table1 => cc.select.f(table1))(cc.select.shape)
      }
    }

    val query1 = ccdd.aabb(permissionTq1, catTq1).result

    db.run(query1).map(s => println("输出：" + s)).futureValue(oneSecondTimeOut)

  }

}