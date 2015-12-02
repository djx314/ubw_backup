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
  permissionName: String = "",
  describe: String = ""
)

class PermissionTable(tag: slick.driver.H2Driver.api.Tag) extends Table[Permission](tag, "S_PERMISSION_TYPE") {
  def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
  def name = column[String]("TYPE_NAME")
  def typeName = column[String]("TYPE_NICK_NAME")
  def describe = column[String]("TYPE_DESCRIBE")

  def * = (id.?, name, typeName, describe) <> (Permission.tupled, Permission.unapply _)
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

  val oneSecondTimeOut = Timeout(Span(1000L, Millis))

  before {
    val aa = permissionTq1 += Permission(
      name = "aa",
      permissionName = "bb",
      describe = "cc"
    )
    try {
      db.run {
        permissionTq1.schema.create >>
          aa
      }.futureValue(oneSecondTimeOut)
    } catch {
      case e: Exception => e.printStackTrace
    }
  }

  after {
    db.run((permissionTq1.schema).drop).futureValue(oneSecondTimeOut)
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

    case class SqlFilter[S, R <: Rep[_] : CanBeQueryCondition](f: S => R) {

      val wt = implicitly[CanBeQueryCondition[R]]
      def myFilter[U, A[_]](query: Query[S, U, A]): Query[S, U, A] = {
        query.filter(table1 => f(table1))(wt)
      }

    }
    case class SqlOrder[S, R](f: S => R)(implicit val wt: R => Ordered) {

      def myOrder[U, A[_]](query: Query[S, U, A]): Query[S, U, A] = {
        query.sortBy(table1 => wt(f(table1)))
      }

    }
    case class SqlSelect[S, T, U](select: S => T)(implicit val shape: Shape[_ <: FlatShapeLevel, T, U, T])
    case class SqlWrapper[S, T, U](
      select: SqlSelect[S, T, U],
      filters: List[SqlFilter[S, _ <: Rep[_]]] = Nil,
      orders: List[SqlOrder[S, _]] = Nil
    ) {

      def where[R <: Rep[_] : CanBeQueryCondition](f: S => R) = {
        val filter1 = SqlFilter(f)
        this.copy(filters = filter1 :: this.filters)
      }

      def order_by[R <% Ordered](f: S => R) = {
        val order1 = SqlOrder(f)
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
          eachFilter.myFilter(fQuery)
        })
        val sortQuery = bb.orders.foldLeft(filterQuery)((fQuery, eachOrder) => {
          eachOrder.myOrder(fQuery)
        })
        sortQuery.map(table1 => bb.select.select(table1))(bb.select.shape)
      }
    }

    val query = aabb.aabb(permissionTq1).result

    db.run(query).map(println)

  }

}