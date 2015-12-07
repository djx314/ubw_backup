package org.xarcher.ubw.nmlgb

import java.sql.ResultSet

import org.h2.jdbcx.JdbcDataSource
import org.scalatest._
import org.scalatest.concurrent.PatienceConfiguration.Timeout
import org.scalatest.concurrent._
import org.scalatest.time.{Millis, Span}
import slick.ast.TypedType
import slick.lifted.{TupleShape, CanBeQueryCondition, Ordered}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.postfixOps
import scala.language.existentials
import slick.driver.H2Driver.api._

import scala.reflect.ClassTag

/**
  * Created by djx314 on 15-6-22.
  */

case class RepDefaultValue[T](value: T)

trait SlickData {

  val property: String
  type DataType
  val data: DataType

  override def toString = s"SlickData(property=$property,data=$data)"

}

case class SlickDataGen[T](override val property: String, override val data: T) extends SlickData {

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

    trait SqlRep[S] {
      type R
      type T
      type G
      val proName: String
      val f: S => R
      val shape: Shape[_ <: FlatShapeLevel, R, T, G]
    }

    implicit class miaolegemiRepExtensionMethod[S1, R1](repLike: S1 => R1) {

      def as[T1, G1](columnName: String)(implicit shape1: Shape[_ <: FlatShapeLevel, R1, T1, G1]) = {
        new SqlRep[S1] {
          type R = R1
          type T = T1
          type G = G1
          val proName = columnName
          val f = repLike
          val shape = shape1
        }
      }

    }

    case class SqlWrapper[S](
      select: List[SqlRep[S]],
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

      val repGens = {
        select match {
          case head :: tail =>
            tail.foldLeft(SelectRep.head(head))((repGen, eachSelect) => {
              repGen.append(eachSelect)
            })
          case _ =>
            throw new Exception("喵了个咪")
        }
      }

      case class DataGen(list: () => List[SlickData], map: () => Map[String, SlickData])

      def queryResult[E[_]](query: Query[S, _, Seq]): DBIO[Seq[DataGen]] = {
        val filterQuery = filters.foldLeft(query)((fQuery, eachFilter) => {
          fQuery.filter(eachFilter.convert)(eachFilter.wt)
        })
        val sortQuery = orders.foldLeft(filterQuery)((fQuery, eachOrder) => {
          fQuery.sortBy(table1 => eachOrder.wt(eachOrder.convert(table1)))
        })
        sortQuery.map(repGens.repGen(_))(repGens.shape).result.map(s => s.map(t => {
          val listPre = () => repGens.listGen(t)
          val mapPre = () => repGens.mapGen(t)
          DataGen(list = listPre, map = mapPre)
        }))
      }

    }

    object select {

      def apply[S, T, U](columns: SqlRep[S]*) = {
        SqlWrapper(
          select = columns.toList
        )
      }

    }

    trait SelectRep[S] {
      type ColType
      type ValType
      type TargetColType
      val shape: Shape[_ <: FlatShapeLevel, ColType, ValType, TargetColType]
      val listGen: ValType => List[SlickData]
      val mapGen: ValType => Map[String, SlickData]
      val repGen: S => ColType

      def append(baseRep: SqlRep[S]): SelectRep[S] = {
        type ColType1 = (ColType, baseRep.R)
        type ValType1 = (ValType, baseRep.T)
        type TargetColType1 = (TargetColType, baseRep.G)
        val shape1 = new TupleShape[FlatShapeLevel, ColType1, ValType1, TargetColType1](shape, baseRep.shape)
        val listGen1: ValType1 => List[SlickData] = (newValue) => {
          val baseList = listGen(newValue._1)
          val appendValue = newValue._2
          val appendSlickData = new SlickData {
            override val property = baseRep.proName
            override type DataType = baseRep.T
            override val data = appendValue
          }
          baseList ::: appendSlickData :: Nil
        }
        val mapGen1: ValType1 => Map[String, SlickData] = (newValue) => {
          val baseList = mapGen(newValue._1)
          val appendValue = newValue._2
          val appendSlickData = new SlickData {
            val property = baseRep.proName
            type DataType = baseRep.T
            val data = appendValue
          }
          baseList + (baseRep.proName -> appendSlickData)
        }
        val repGen1: S => ColType1 = sourceTable => {
          val initCols = repGen(sourceTable)
          val newCol = baseRep.f(sourceTable)
          initCols -> newCol
        }

        new SelectRep[S] {
          override type ColType = ColType1
          override type ValType = ValType1
          override type TargetColType = TargetColType1
          override val shape = shape1
          override val listGen = listGen1
          override val mapGen = mapGen1
          override val repGen = repGen1
        }
      }
    }

    object SelectRep {

      def head[S](baseRep: SqlRep[S]): SelectRep[S] = {
        new SelectRep[S] {
          override type ColType = Tuple1[baseRep.R]
          override type ValType = Tuple1[baseRep.T]
          override type TargetColType = Tuple1[baseRep.G]
          override val shape = new TupleShape[FlatShapeLevel, Tuple1[baseRep.R], Tuple1[baseRep.T], Tuple1[baseRep.G]](baseRep.shape)
          override val listGen = (baseVal: ValType) => {
            val initValue = new SlickData {
              override val property = baseRep.proName
              override type DataType = baseRep.T
              override val data = baseVal._1
            }
            initValue :: Nil
          }
          override val mapGen = (baseVal: ValType) => {
            val initValue = new SlickData {
              override val property = baseRep.proName
              override type DataType = baseRep.T
              override val data = baseVal._1
            }
            Map(baseRep.proName -> initValue)
          }
          override val repGen = (baseTable: S) => {
            Tuple1(baseRep.f(baseTable))
          }
        }
      }

    }

    def bb = {
      select(
        ((table1: PermissionTable) => {
          (table1.typeName)
        }) as "喵了个咪",
        ((table1: PermissionTable) => {
          (table1.typeName)
        }) as "喵了个咪11",
        ((table1: PermissionTable) => {
          (table1.describe)
        }) as "喵了个咪22",
        ((table1: PermissionTable) => {
          (table1.id)
        }) as "喵了个咪33",
        ((table1: PermissionTable) => {
          (table1.describe)
        }) as "喵了个咪44"
      )
      .where (table1 => table1.describe === "cc")
      .order_by (table1 => table1.name)
    }

    object aabb {
      def aabb(query: Query[PermissionTable, Permission, Seq]) = {
        bb.queryResult(query)
      }
    }

    val query = aabb.aabb(permissionTq1)

    db.run(query).map(s => println(s.toList.map(t => t.map()))).futureValue(oneSecondTimeOut)

    def cc = {
      select(
        ((table1: (PermissionTable, CatTable)) => {
          table1._1.name
        }) as "afdhrhtrhtrh",
        ((table1: (PermissionTable, CatTable)) => {
          table1._1.typeName
        }) as "cccccccccccccc",
        ((table1: (PermissionTable, CatTable)) => {
          table1._2.id
        }) as "aaaaaaaaaaaaaa",
        ((table1: (PermissionTable, CatTable)) => {
          table1._2.miao
        }) as "bbbbbbbbbbbbbb"
      )
      .where { case (table1, table2) => table1.describe === "cc" }
      .where { case (table1, table2) => table2.wang === table1.name }
      .order_by { case (table1, table2) => table2.wang }
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

    db.run(query1).map(s => println(s.toList.map(t => t.list()))).futureValue(oneSecondTimeOut)

  }

}