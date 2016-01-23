package org.xarcher.ubw.mapper

import slick.driver.JdbcActionComponent
import slick.lifted._
import slick.dbio._

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

trait Aaaaa {

  def insertSeq[U, A, C[_]](query: Query[U, A, C])(data: Iterable[A])
  (implicit cv: Query[U, A, C] => JdbcActionComponent#InsertActionExtensionMethods[A]): DBIO[Option[Int]] = {
    cv(query) ++= data
  }

}

object KKKK extends Aaaaa {

  import slick.driver.MySQLDriver.api._
  import scala.concurrent.ExecutionContext.Implicits.global

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

  val tableQuery = TableQuery[PermissionTable]

  insertSeq(tableQuery)(Seq.empty[Permission])

}