package org.xarcher.ubw.slick

import java.sql.ResultSet

import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent._
import org.scalatest.time.{Millis, Span}
import slick.ast._
import slick.jdbc.JdbcType

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds
import scala.language.implicitConversions
import slick.driver.H2Driver.api._
import slick.lifted.{AbstractTable, Ordered, CanBeQueryCondition}

import scala.reflect.ClassTag

/**
  * Created by djx314 on 15-6-22.
  */

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

  implicit def slickDataColumnType[T : BaseColumnType]: BaseColumnType[SlickDataGen[T]] =
    MappedColumnType.base[SlickDataGen[T], T](
      dt => dt.data,
      ts => SlickDataGen(ts)
    )

  import slick.driver.H2Driver._

  implicit def ggdgjhigherugihuriehg[T](implicit jdbcType: ColumnType[T]): ColumnType[SlickDataGen[Option[T]]] = {
    class AA/*(implicit override val classTag: ClassTag[SlickDataGen[Option[T]]])*/extends MappedJdbcType[SlickDataGen[Option[T]], T]()(jdbcType, ClassTag[SlickDataGen[Option[T]]](classOf[SlickDataGen[Option[T]]]))/*TypedType[SlickDataGen[Option[T]]] with ScalaType[SlickDataGen[Option[T]]] with AtomicType*/{
      /*def nullable: Boolean = true//jdbcType.scalaType.nullable
      def ordered: Boolean = jdbcType.optionType.scalaType.ordered
      def scalaOrderingFor(ord: Ordering): scala.math.Ordering[SlickDataGen[Option[T]]] = new scala.math.Ordering[SlickDataGen[Option[T]]] {
        val uOrdering = jdbcType.optionType.scalaType.scalaOrderingFor(ord)//baseType.scalaType.scalaOrderingFor(ord)
        def compare(x: SlickDataGen[Option[T]], y: SlickDataGen[Option[T]]): Int = uOrdering.compare(x.data, y.data)
      }*/
      println("22" * 100)
      override def map(t: SlickDataGen[Option[T]]): T = t.data match {
        case Some(s) => s
        case None => ???
      }
      override def comap(u: T): SlickDataGen[Option[T]] = {
        SlickDataGen(Option(u))
      }
      override def wasNull(r: ResultSet, idx: Int) = {
        println("11" * 100)
        println(r)
        println(r.wasNull())
        r.wasNull()
        false
      }
    }
    new AA()
  }

  /*implicit def slickDataColumnOptionType[T](implicit typedType: JdbcType[T]): BaseColumnType[SlickDataGen[Option[T]]] = {
    import slick.driver.H2Driver._

    class MappedColumnType11(toBase:  => U, toMapped: U => S)(implicit override val classTag: ClassTag[S], val jdbcType: JdbcType[U]) extends TypedType[S] with ScalaType[S] with AtomicType/*ScalaType[T] with BaseTypedType[T]*/{
      def nullable: Boolean = true//jdbcType.scalaType.nullable
      def ordered: Boolean = jdbcType.scalaType.ordered
      def scalaOrderingFor(ord: Ordering): scala.math.Ordering[S] = new scala.math.Ordering[S] {
        val uOrdering = jdbcType.scalaType.scalaOrderingFor(ord)
        def compare(x: S, y: S): Int = uOrdering.compare(toBase(x), toBase(y))
      }
      def comap(u: U): S = toMapped(u)
      def map(t: S): U = toBase(t)
    }

    //new MappedColumnType11[SlickDataGen[Option[T]], T](dt => throw new SlickException("SlickDataGen 这个类只提供读的功能，写的功能暂不提供")/*dt.data.get*/, ts => SlickDataGen(Option(ts)))
    ???
  }*/

  implicit class columnTypeToSlickDataColumnType[T](baseRep: Rep[T])(implicit typedType: TypedType[SlickDataGen[T]]) {
    def miaolegemi = {
      val convert = SimpleFunction.unary[T, SlickDataGen[T]]("")
      convert(baseRep)
    }
  }

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
      typeName = None,//Option("bb"),
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
      val aa = implicitly[ColumnType[SlickDataGen[Option[String]]]]
      val bb = ggdgjhigherugihuriehg[String]
      select(
        (table1: (PermissionTable, CatTable)) => {
          (table1._1.name.miaolegemi, {
            //println(table1._1.typeName.getClass)
            table1._1.typeName.miaolegemi
          }, table1._2.id.miaolegemi)
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

    try {
      db.run(query1).map(s => println("输出：" + s)).futureValue(oneSecondTimeOut)
    } catch {
      case e: Exception => e.printStackTrace
    }

  }

}