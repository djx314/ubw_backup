package org.xarcher.ubw.slick

import java.sql.ResultSet

import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent._
import org.scalatest.time.{Millis, Span}
import slick.ast.TypedType
import slick.lifted.{CanBeQueryCondition, Ordered}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds
import scala.language.implicitConversions
import slick.driver.H2Driver.api._

import scala.reflect.ClassTag

/**
  * Created by djx314 on 15-6-22.
  */

case class RepDefaultValue[T](value: T)

trait SlickData {

  type DataType
  val data: DataType

  override def toString = s"SlickData($data)"

}

case class SlickDataGen[T](override val data: T) extends SlickData {

  override type DataType = T

}

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

  implicit object defaultRepStringValue extends RepDefaultValue("喵了个咪，真想日死 szeiger 偏要让我用这种旁门左道的方法")
  implicit object defaultRepIntValue extends RepDefaultValue(Int.MinValue)
  implicit object defaultRepLongValue extends RepDefaultValue(Long.MinValue)

  implicit def slickDataColumnType[T : BaseColumnType]: BaseColumnType[SlickDataGen[T]] =
    MappedColumnType.base[SlickDataGen[T], T](
      dt => dt.data,
      ts => SlickDataGen(ts)
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
          SlickDataGen(None)
        else
          SlickDataGen(Option(u))
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
  }

  lazy val db = {
    val datasource = new JdbcDataSource()
    datasource.setUrl(s"jdbc:h2:mem:mlgmTest;DB_CLOSE_DELAY=-1")
    Database.forDataSource(datasource)
  }

  val permissionTq1 = TableQuery[PermissionTable]
  val catTq1 = TableQuery[CatTable]

  val oneSecondTimeOut = Timeout(Span(1000L, Millis))

  before {
    val aa = permissionTq1 += Permission(
      name = "bb",
      typeName = /*None,*/Option("bb"),
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

      def apply[S, T, U](columns: S => T)(implicit shape: Shape[_ <: slick.lifted.FlatShapeLevel, T, U, T]) = {
        val select1 = SqlSelect(columns)
        SqlWrapper(
          select = select1
        )
      }

    }

    def bb = {
      select(
        (table1: PermissionTable) => {
          (table1.typeName, table1.name)
        }
      )
      .where (table1 => table1.describe === "cc")
      .order_by (table1 => table1.name)
    }

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
          (table1._1.name.miaolegemi, table1._1.typeName.miaolegemi, table1._2.id.miaolegemi, table1._2.miao.miaolegemi)
        }
      )
      .where { case (table1, table2) => table1.describe === "cc" }
      .where { case (table1, table2) => table2.wang === table1.name }
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

    db.run(query1).map(s => println("输出：" + s.head)).futureValue(oneSecondTimeOut)
    db.run(permissionTq1.map(_.typeName.miaolegemi).result).map(s => println("输出1111：" + s.head)).futureValue(oneSecondTimeOut)

  }

}