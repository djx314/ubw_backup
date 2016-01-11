package org.xarcher.ubw

import io.circe._, io.circe.generic.auto._, io.circe.syntax._

import slick.lifted._

import scala.reflect.runtime.universe._

/**
  * Created by Administrator on 2015/12/9.
  */
package object wrapper {

  implicit class miaolegemiRepExtensionMethod[S1, R1](repLike: S1 => R1) {

    def as_ext[T1 : WeakTypeTag, G1](columnName: String)(implicit shape1: Shape[_ <: FlatShapeLevel, R1, T1, G1], jsonEncoder1: Encoder[T1]): SqlRep[S1, R1, G1] = {
      new SqlRep[S1, R1, G1] {
        //override type R = R1
        override type T = T1
        //override type G = G1
        override val valueTypeTag = implicitly[WeakTypeTag[T1]]
        override val proName = columnName
        override val isHidden = false
        override val isDefaultDesc = true
        override val f = repLike
        override val shape = shape1
        override val jsonEncoder = jsonEncoder1
      }
    }

  }

  implicit class miaolegemiRepExtensionMethod1111[R1](repLike: R1) {

    def as[T1 : WeakTypeTag, G1](columnName: String)(implicit shape1: Shape[_ <: FlatShapeLevel, R1, T1, G1]): SqlRep[Any, R1, G1] = ???

  }

  val Ubw = macros.Ubw

}