package org.xarcher.ubw.mapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._
import org.xarcher.cpoi.{CellData, WriteableCellOperationAbs}

import slick.dbio._
import slick.lifted._
import slick.profile.BasicProfile
import scala.concurrent.{ExecutionContext, Await}
import scala.language.higherKinds
import scala.reflect.runtime.universe._

trait SlickData {

  val property: String
  type DataType
  val data: DataType

  val jsonEncoder: Encoder[DataType]

  def toJson: Json = {
    data.asJson(jsonEncoder)
  }

  val typeTag: WeakTypeTag[DataType]

  val poiWriter: WriteableCellOperationAbs[DataType]
  lazy val cellData = CellData(Option(data))(poiWriter)

  val isHidden: Boolean

  override def toString = s"SlickData(property=$property,data=$data,type=${typeTag.tpe})"

}

trait SqlRepOrder[TargetType] {

  type RepType
  val typeConvert: TargetType <:< Rep[RepType]
  val wt: Rep[RepType] => ColumnOrdered[RepType]

}

trait SqlRep[R, T, G] {

  val rep: R

  val proName: String
  val isHidden: Boolean
  val isDefaultDesc: Boolean
  val shape: Shape[_ <: FlatShapeLevel, R, T, G]
  val valueTypeTag: WeakTypeTag[T]
  val jsonEncoder: Encoder[T]
  val poiWritter: WriteableCellOperationAbs[T]
  /**
    * 如果同时拥有 orderTarget 和 ordereImplicit，以 orderTarget 为先
    */
  val orderTargetName: Option[String] = None
  val sqlOrder: Option[SqlRepOrder[G]] = None

  def hidden(isHidden: Boolean = this.isHidden): this.type = {
    val isHidden1 = isHidden
    this.copy(isHidden = isHidden1)
  }

  def order[K](isDefaultDesc: Boolean)(implicit columnGen: G <:< Rep[K], wtImplicit: Rep[K] => ColumnOrdered[K]): this.type = {
    val isDefaultDesc1 = isDefaultDesc
    val sqlOrder1 = new SqlRepOrder[G] {
      override type RepType = K
      override val typeConvert = columnGen
      override val wt = wtImplicit
    }
    this.copy(sqlOrder = Option(sqlOrder1), isDefaultDesc = isDefaultDesc1)
  }

  def orderTarget(targetName: String, isDefaultDesc: Boolean): this.type = {
    val targetName1 = targetName
    val isDefaultDesc1 = isDefaultDesc
    this.copy(orderTargetName = Option(targetName1), isDefaultDesc = isDefaultDesc1)
  }

  def copy(proName: String = this.proName, isHidden: Boolean = this.isHidden, isDefaultDesc: Boolean = this.isDefaultDesc, rep: R = this.rep,
           orderTargetName: Option[String] = this.orderTargetName, sqlOrder: Option[SqlRepOrder[G]] = this.sqlOrder): this.type = {
    val proName1 = proName
    val isHidden1 = isHidden
    val isDefaultDesc1 = isDefaultDesc
    val rep1 = rep
    val shape1 = this.shape
    val valueTypeTag1 = this.valueTypeTag
    val jsonEncoder1 = this.jsonEncoder
    val orderTargetName1 = orderTargetName
    val sqlOrder1 = sqlOrder
    val poiWritter1 = this.poiWritter
    new SqlRep[R, T, G] {
      override val proName = proName1
      override val isHidden = isHidden1
      override val isDefaultDesc = isDefaultDesc1
      override val rep = rep1
      override val shape = shape1
      override val valueTypeTag = valueTypeTag1
      override val jsonEncoder = jsonEncoder1
      override val orderTargetName = orderTargetName1
      override val sqlOrder = sqlOrder1
      override val poiWritter = poiWritter1
    }.asInstanceOf[this.type]
  }

}

trait SelectRep {
  type ColType
  type ValType
  type TargetColType
  val shape: Shape[_ <: FlatShapeLevel, ColType, ValType, TargetColType]
  val listGen: ValType => List[SlickData]
  val mapGen: ValType => Map[String, SlickData]
  val rep: ColType

  def append[RT, G1, T1](baseRep: SqlRep[RT, T1, G1]): SelectRep = {
    type ColType1 = (ColType, RT)
    type ValType1 = (ValType, T1)
    type TargetColType1 = (TargetColType, G1)
    val shape1 = new TupleShape[FlatShapeLevel, ColType1, ValType1, TargetColType1](shape, baseRep.shape)
    val listGen1: ValType1 => List[SlickData] = (newValue) => {
      val baseList = listGen(newValue._1)
      val appendValue = newValue._2
      val appendSlickData = new SlickData {
        override val property = baseRep.proName
        override type DataType = T1
        override val data = appendValue
        override val jsonEncoder = baseRep.jsonEncoder
        override val typeTag = baseRep.valueTypeTag
        override val poiWriter = baseRep.poiWritter
        override val isHidden = baseRep.isHidden
      }
      baseList ::: appendSlickData :: Nil
    }
    val mapGen1: ValType1 => Map[String, SlickData] = (newValue) => {
      val baseList = mapGen(newValue._1)
      val appendValue = newValue._2
      val appendSlickData = new SlickData {
        override val property = baseRep.proName
        override type DataType = T1
        override val data = appendValue
        override val jsonEncoder = baseRep.jsonEncoder
        override val typeTag = baseRep.valueTypeTag
        override val poiWriter = baseRep.poiWritter
        override val isHidden = baseRep.isHidden
      }
      baseList + (baseRep.proName -> appendSlickData)
    }
    val repGen1: ColType1 = rep -> baseRep.rep

    new SelectRep {
      override type ColType = ColType1
      override type ValType = ValType1
      override type TargetColType = TargetColType1
      override val shape = shape1
      override val listGen = listGen1
      override val mapGen = mapGen1
      override val rep = repGen1
    }
  }
}

object SelectRep {

  def head[RT, G1, T1](baseRep: SqlRep[RT, T1, G1]): SelectRep = {
    new SelectRep {
      override type ColType = Tuple1[RT]
      override type ValType = Tuple1[T1]
      override type TargetColType = Tuple1[G1]
      override val shape = new TupleShape[FlatShapeLevel, Tuple1[RT], Tuple1[T1], Tuple1[G1]](baseRep.shape)
      override val listGen = (baseVal: ValType) => {
        val initValue = new SlickData {
          override val property = baseRep.proName
          override type DataType = T1
          override val data = baseVal._1
          override val jsonEncoder = baseRep.jsonEncoder
          override val typeTag = baseRep.valueTypeTag
          override val poiWriter = baseRep.poiWritter
          override val isHidden = baseRep.isHidden
        }
        initValue :: Nil
      }
      override val mapGen = (baseVal: ValType) => {
        val initValue = new SlickData {
          override val property = baseRep.proName
          override type DataType = T1
          override val data = baseVal._1
          override val jsonEncoder = baseRep.jsonEncoder
          override val typeTag = baseRep.valueTypeTag
          override val poiWriter = baseRep.poiWritter
          override val isHidden = baseRep.isHidden
        }
        Map(baseRep.proName -> initValue)
      }
      override val rep = Tuple1(baseRep.rep)
    }
  }

}

case class DataGen(list: () => List[SlickData], map: () => Map[String, SlickData])

trait UQuery[A, B, C, D] {

  val query: Query[A, B, Seq]
  lazy val queryGen: Query[A, B, Seq] => Query[C, D, Seq] = ???
  lazy val resultConvert: D => DataGen = ???

  def uresult(
    implicit streamEv: Query[C, D, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[D], D],
    ec: ExecutionContext
  ): DBIO[Seq[DataGen]] = {
    streamEv(queryGen(query)).result.map(s => s.map(t => resultConvert(t)))(ec)
  }

  def flatMap[F, T](f: A => UQuery[F, T, C, D])
  : UQuery[F, T, C, D] = {
    val query1 = this.query
    val query2 = query.flatMap(s => f(s).query)
    //val queryGen1 = this.queryGen
    val resultConvert1 = this.resultConvert
    new UQuery[F, T, C, D] {
      override val query = query2
      override lazy val queryGen = (minQuery: Query[F, T, Seq]) => {
        query1.flatMap(s => f(s).queryGen(minQuery))
      }
      override lazy val resultConvert = resultConvert1
    }
  }

  /*def map[F, G, T](f: E => F)(implicit shape: Shape[_ <: FlatShapeLevel, F, T, G]): UQuery[G, T, C] = {
    val query1 = this.query
    new UQuery[G, T, C] {
      override val query = query1.map(f)(shape)
    }
  }*/

  /*def map(sqlRepsGen: List[SqlRep[_, _, _]]): UQuery[_, _] = {
    val thisQuery = this.query
    val repList = sqlRepsGen
    val selectRep: SelectRep = repList match {
      case head :: tail =>
        tail.foldLeft(SelectRep.head(head))((head, toAppend) => {
          head.append(toAppend)
        })
      case _ => ???
    }
    val query1 = Query(()).map(_ => selectRep.rep)(selectRep.shape)
    new UQuery[selectRep.TargetColType, selectRep.ValType] {
      override val query = query1
      override def resultConvert = data => DataGen(() => selectRep.listGen(data), () => selectRep.mapGen(data))
    }
  }*/

  /*def filter[T <: Rep[_] : CanBeQueryCondition](f: E => T): UQuery[E, U, C] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    val query1 = this.query
    new UQuery[E, U, C] {
      override val query = query1.filter(f)(cv)
    }
  }

  def filterNot[T <: Rep[_] : CanBeQueryCondition](f: E => T): UQuery[E, U, C] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    val query1 = this.query
    new UQuery[E, U, C] {
      override val query = query1.filterNot(f)(cv)
    }
  }

  def withFilter[T : CanBeQueryCondition](f: E => T): UQuery[E, U, C] = {
    val cv = implicitly[CanBeQueryCondition[T]]
    val query1 = this.query
    new UQuery[E, U, C] {
      override val query = query1.withFilter(f)(cv)
    }
  }*/

  /*def result(implicit streamEv: Query[E, U, Seq] => BasicProfile#StreamingQueryActionExtensionMethods[Seq[U], U])
  : BasicProfile#StreamingDriverAction[Seq[U], U, Effect.Read] = {
    streamEv(query).result
  }*/

}

object UQuery {

  def apply(selectRep: SelectRep): UQuery[selectRep.TargetColType, selectRep.ValType, selectRep.TargetColType, selectRep.ValType] = {
    new UQuery[selectRep.TargetColType, selectRep.ValType, selectRep.TargetColType, selectRep.ValType] {
      override val query = Query(()).map(_ => selectRep.rep)(selectRep.shape)
      override lazy val queryGen = (query: Query[selectRep.TargetColType, selectRep.ValType, Seq]) => query
      override lazy val resultConvert = (data: selectRep.ValType) => DataGen(() => selectRep.listGen(data), () => selectRep.mapGen(data))
    }
  }

}

object TestCompile extends App {

  import org.h2.jdbcx.JdbcDataSource

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

  /*println{
    Await.result(db.run {
      (for {
        permission <- UQuery(permissionTq1) if permission.id > 46L
        cat <- UQuery(catTq1)//if cat.id > 23L
      } yield {
        permission -> cat
      }).result
    }, scala.concurrent.duration.Duration.Inf)
  }*/

  Await.result(db.run((permissionTq1.schema ++ catTq1.schema).drop), scala.concurrent.duration.Duration.Inf)

}