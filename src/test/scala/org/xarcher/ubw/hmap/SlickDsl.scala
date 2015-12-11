package org.xarcher.ubw.wrapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._

import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent._
import org.scalatest.time.{Millis, Span}
import org.xarcher.ubw.macros.Ubw

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

  /*implicit object defaultRepStringValue extends RepDefaultValue("喵了个咪，真想日死 szeiger 偏要让我用这种旁门左道的方法")
  implicit object defaultRepIntValue extends RepDefaultValue(Int.MinValue)
  implicit object defaultRepLongValue extends RepDefaultValue(Long.MinValue)

  implicit def slickDataColumnType[T : BaseColumnType]: BaseColumnType[SlickDataGen[T]] =
    MappedColumnType.base[SlickDataGen[T], T](
      dt => dt.data,
      ts => SlickDataGen("123", ts)
    )

  import slick.driver.H2Driver._

  implicit def ggdgjhigherugihuriehg[T](implicit jdbcType: ColumnType[T], classTag: ClassTag[SlickDataGen[Option[T]]], defaultValue: RepDefaultValue[T]): ColumnType[SlickDataGen[Option[T]]] = {
    class AA extends MappedJdbcType[SlickDataGen[Option[T]], T]() {
      override def map(t: SlickDataGen[Option[T]]): T = t.data match {
        case Some(s) => s
        case None => null.asInstanceOf[T]
      }
      override def comap(u: T): SlickDataGen[Option[T]] = {
        if (u == defaultValue.value)
          SlickDataGen("123", None)
        else
          SlickDataGen("123", Option(u))
      }
      override def getValue(r: ResultSet, idx: Int) = {
        val v = tmd.getValue(r, idx)
        val baseValue = if((v.asInstanceOf[AnyRef] eq null) || tmd.wasNull(r, idx))
          defaultValue.value
        else
          v
        comap(baseValue)
      }
      override def wasNull(r: ResultSet, idx: Int) = {
        r.wasNull()
      }
    }
    new AA()
  }

  implicit class columnTypeToSlickDataColumnType[T](baseRep: Rep[T])(implicit typedType: TypedType[SlickDataGen[T]]) {
    def miaolegemi = {
      val convert = SimpleFunction.unary[T, SlickDataGen[T]]("")
      convert(baseRep)
    }
  }*/

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



    def bb = {
      select(
        ((table1: PermissionTable) => {
          table1.typeName
        }) as_ext "喵了个咪",
        ((table1: PermissionTable) => {
          table1.typeName
        }) as_ext "喵了个咪11",
        ((table1: PermissionTable) => {
          table1.describe
        }) as_ext "喵了个咪22",
        ((table1: PermissionTable) => {
          table1.id
        }) as_ext "喵了个咪33",
        ((table1: PermissionTable) => {
          table1.describe
        }) as_ext "喵了个咪44"
      )
      .where_ext (table1 => table1.describe === "cc")
      .order_by_ext (table1 => table1.name)
    }

    object aabb {
      def aabb(query: Query[PermissionTable, Permission, Seq]) = {
        bb.queryResult(query)
      }
    }

    val query = aabb.aabb(permissionTq1)

    db.run(query).map(s => println(s.map(t => t.map()))).futureValue(oneSecondTimeOut)

    def cc = {
      select(
        ((table1: (PermissionTable, CatTable)) => {
          table1._1.name
        }) as_ext "afdhrhtrhtrh",
        ((table1: (PermissionTable, CatTable)) => {
          table1._1.typeName
        }) as_ext "cccccccccccccc",
        ((table1: (PermissionTable, CatTable)) => {
          table1._2.id
        }) as_ext "aaaaaaaaaaaaaa",
        ((table1: (PermissionTable, CatTable)) => {
          table1._2.miao
        }) as_ext "bbbbbbbbbbbbbb"
      )
      .where_ext { case (table1, table2) => table1.describe === "cc" }
      .where_ext { case (table1, table2) => table2.wang === table1.name }
      .order_by_ext { case (table1, table2) => table2.wang }
    }

    object ccdd {
      def aabb(permissionTq: Query[PermissionTable, Permission, Seq], catTq: Query[CatTable, Cat, Seq]) = {
        val bbccc = for {
          pTq <- permissionTq
          cTq <- catTq
        } yield pTq -> cTq
        cc.queryResult(bbccc)
      }
    }

    val query1 = ccdd.aabb(permissionTq1, catTq1)

    db.run(query1).map(s => println(s.map(t => t.list()))).futureValue(oneSecondTimeOut)

    import Ubw._

    def dd = from {
      (permission: PermissionTable, cat: CatTable) =>
        select(permission as "喵了个咪", permission.name as "喵", cat.wang as "十六夜的樱丘", cat as "卖了个萌")
          .where(permission.describe like "%%")
          .where(permission.describe like "%%")
          .where(cat.wang like "%%")
          .where { cat.wang === permission.name }
          .order_by(cat.wang)
          .order_by(permission.describe)
    }

    db.run(dd).map(s => println(s.map(t => t.list().map(u => u.property -> u.toJson)))).futureValue(oneSecondTimeOut)


  }

}