package net.scalax.ubw.mapper

import slick.lifted._

trait SelectRep {
  type ColType
  type ValType
  type TargetColType
  val shape: Shape[_ <: FlatShapeLevel, ColType, ValType, TargetColType]
  val listGen: ValType => List[SlickData]
  val mapGen: ValType => Map[String, SlickData]
  val rep: ColType
  val orderGen: Map[String, TargetColType => ColumnOrdered[_]]
  val baseSqlReps: List[SqlRep[_, _, _]]

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
    val orderGen1: Map[String, TargetColType1 => ColumnOrdered[_]] = {
      val oldMap = orderGen.map { case (key, convert) => {
        val convert1: TargetColType1 => ColumnOrdered[_] = {
          case (currentTargetCol, newCol) =>
            convert(currentTargetCol)
        }
        key -> convert1
      } }
      (for {
        sqlOrder <- baseRep.sqlOrder
      } yield {
        val newConvert: TargetColType1 => ColumnOrdered[_] = {
          case (currentTargetCol, newCol) =>
            sqlOrder.wt(sqlOrder.typeConvert(newCol))
        }
        oldMap + (baseRep.proName -> newConvert)
      })
        .getOrElse(oldMap)
    }
    val repGen1: ColType1 = rep -> baseRep.rep
    val baseSqlReps1 = (baseRep :: baseSqlReps.reverse).reverse

    new SelectRep {
      override type ColType = ColType1
      override type ValType = ValType1
      override type TargetColType = TargetColType1
      override val shape = shape1
      override val listGen = listGen1
      override val mapGen = mapGen1
      override val rep = repGen1
      override val orderGen = orderGen1
      override val baseSqlReps = baseSqlReps1
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
      override val orderGen = {
        (for {
          sqlOrder <- baseRep.sqlOrder
        } yield {
          val newConvert: TargetColType => ColumnOrdered[_] = {
            case Tuple1(initCol) =>
              sqlOrder.wt(sqlOrder.typeConvert(initCol))
          }
          Map(baseRep.proName -> newConvert)
        })
          .getOrElse(Map.empty[String, TargetColType => ColumnOrdered[_]])
      }
      override val baseSqlReps = baseRep :: Nil
    }
  }

}