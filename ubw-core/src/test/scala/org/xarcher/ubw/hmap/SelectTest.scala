package org.xarcher.ubw.slick

import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent._
import org.scalatest.time.{Millis, Span}
import slick.lifted.{Ordered, CanBeQueryCondition}

import scala.language.higherKinds
import slick.driver.H2Driver.api._

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
    try {
      db.run(permissionTq1.schema.create).futureValue(oneSecondTimeOut)
    } catch {
      case e: Exception => e.printStackTrace
    }
  }

  after {
    db.run((permissionTq1.schema).drop).futureValue(oneSecondTimeOut)
  }

  "Small table" should "update some colunms" in {

    val aa = permissionTq1 += Permission(
      name = "aa",
      permissionName = "bb",
      describe = "cc"
    )

    db.run(aa).futureValue(oneSecondTimeOut)

    class Mlgb {
      val permissionTq = permissionTq1
    }

    val mlgb = new Mlgb()

    import mlgb._

    val query = permissionTq.filter(_.name === "aa")

    permissionTq.filter(_.name === "1234")

    println(db.run(query.result).futureValue(oneSecondTimeOut))

    SelectMacro.decodePrintln('喵了个咪)
  }

  "aa" should "bb" in {

    case class SqlFilter[R <: Rep[_] : CanBeQueryCondition](f: () => R) {
      implicit val canBeQueryCondition = implicitly[CanBeQueryCondition[R]]
    }
    case class SqlOrder[R](f: () => R)(implicit val wt: R => Ordered)
    case class SqlSelect[T, U](select: () => T)(implicit val shape: Shape[_ <: ShapeLevel, T, U, T])
    case class SqlWrapper[T, U](
      select: () => SqlSelect[T, U],
      filters: List[() => SqlFilter[_]],
      orders: List[() => SqlOrder[_]]
    )

    def bb(table1: PermissionTable): SqlWrapper[(Rep[String], Rep[String]), (String, String)] = {
      ???
    }

    case class Nmlgb()

    object aabb {
      def aabb(query: TableQuery[PermissionTable]) = {
        query.flatMap(table1 => {
          val wrapper = bb(table1)
          query.map(table2 => wrapper.select().select())(wrapper.select().shape)
        })
      }
    }

  }

}