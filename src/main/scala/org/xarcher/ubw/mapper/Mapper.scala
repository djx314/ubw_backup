package org.xarcher.ubw.mapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._
import org.xarcher.cpoi.{CellData, WriteableCellOperationAbs}
import slick.ast.TypedType

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.existentials
import scala.reflect.runtime.universe._
import slick.dbio._
import slick.driver.{JdbcProfile, JdbcActionComponent}
import slick.lifted._

trait Mapper {

  implicit class ubwQueryExtensionMethodImpl[E, U](query1: Query[E, U, Seq]) {

    def by(columns: SqlRep[E, _, _, _]*): SqlWrapper[E] = {
      SqlWrapper[E](select = columns.toList, query = query1)
    }

  }

  implicit class miaolegemiRepExtensionMethod[S1, R1](repLike: S1 => R1) {

    def as_ext[T1 : WeakTypeTag, G1](columnName: String)(implicit shape1: Shape[_ <: FlatShapeLevel, R1, T1, G1], jsonEncoder1: Encoder[T1], writeOperation: WriteableCellOperationAbs[T1]): SqlRep[S1, R1, T1, G1] = {
      new SqlRep[S1, R1, T1, G1] {
        override val valueTypeTag = implicitly[WeakTypeTag[T1]]
        override val proName = columnName
        override val isHidden = false
        override val isDefaultDesc = true
        override val f = repLike
        override val shape = shape1
        override val jsonEncoder = jsonEncoder1
        override val poiWritter = writeOperation
      }
    }

  }

  implicit class miaolegemiRepExtensionMethod1111[R1](repLike: R1) {

    def as[T1 : WeakTypeTag, G1](columnName: String)(implicit shape1: Shape[_ <: FlatShapeLevel, R1, T1, G1], jsonEncoder1: Encoder[T1], writeOperation: WriteableCellOperationAbs[T1]): SqlRep[Any, R1, T1, G1] = ???

  }

}

object Mapper extends Mapper