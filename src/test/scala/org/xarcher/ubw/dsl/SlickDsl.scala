package org.xarcher.ubw.dsl

import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent._
import org.scalatest.time.{Millis, Span}
import org.xarcher.ubw.macros.Ubw

import scala.collection.convert.Wrappers.SeqWrapper
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.existentials
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.postfixOps
import slick.driver.H2Driver.api._

/**
  * Created by djx314 on 15-6-22.
  */
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

class SelectTest extends FlatSpec
with ScalaFutures
with Matchers
with BeforeAndAfter
with OneInstancePerTest {

  lazy val db = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:ubwTest;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource)
  }

  val permissionTq1 = TableQuery[PermissionTable]
  val catTq1 = TableQuery[CatTable]

  val oneSecondTimeOut = Timeout(Span(1000L, Millis))

  before {
    val aa = permissionTq1 += Permission(
      name = "bb",
      typeName = Option("bb"),
      describe = "cc"
    )
    val aabb = catTq1 += Cat(
      //miao = 2345L,
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

  "aa" should "bb" in {

    val convert = (ee: (PermissionTable, CatTable)) => {
      ee match {
        case (permission, cat) =>
          SqlWrapper.map(permission.describe, cat.id, cat.wang, permission.name)
          .where(permission.typeName === "bb")
          .order_by(cat.miao)
      }
    }

    val aa = for {
      permission <- permissionTq1
      cat <- catTq1
    } yield {
      permission -> cat
    }

    try {
      db.run(SqlWrapper.aaaaResult(aa, convert).result).map(s => println(s + "11" * 100)).futureValue(oneSecondTimeOut)
    } catch {
      case e: Exception => e.printStackTrace
    }

  }

}