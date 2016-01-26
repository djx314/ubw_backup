package net.scalax.ubw.helper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._

import org.xarcher.cpoi.WriteableCellOperationAbs
import net.scalax.ubw.mapper.{QueryToUQueryExtensionMethods, SqlRep}

import scala.reflect.runtime.universe._
import slick.lifted.{FlatShapeLevel, Query, Shape}

trait Implicits {

  implicit class queryToUQueryExtendsionMethodGen[E, U](query: Query[E, U, Seq]) {

    def ubw = new QueryToUQueryExtensionMethods[E, U](query)

  }

  implicit class miaolegemiRepExtensionMethod[R1](repLike: R1) {

    def as[T1: WeakTypeTag, G1](columnName: String)(implicit shape1: Shape[_ <: FlatShapeLevel, R1, T1, G1], jsonEncoder1: Encoder[T1], writeOperation: WriteableCellOperationAbs[T1]): SqlRep[R1, T1, G1] = {
      new SqlRep[R1, T1, G1] {
        override val valueTypeTag = implicitly[WeakTypeTag[T1]]
        override val proName = columnName
        override val isHidden = false
        override val isDefaultDesc = true
        override val rep = repLike
        override val shape = shape1
        override val jsonEncoder = jsonEncoder1
        override val poiWritter = writeOperation
      }
    }

  }

}