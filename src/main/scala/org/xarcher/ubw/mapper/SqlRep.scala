package org.xarcher.ubw.mapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._

import org.xarcher.cpoi.{CellData, WriteableCellOperationAbs}

import scala.reflect.runtime.universe._
import slick.lifted.{FlatShapeLevel, Shape, ColumnOrdered, Rep}


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