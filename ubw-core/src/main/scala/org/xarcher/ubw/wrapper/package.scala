package org.xarcher.ubw

import slick.lifted._

/**
  * Created by Administrator on 2015/12/9.
  */
package object wrapper {

  implicit class miaolegemiRepExtensionMethod[S1, R1](repLike: S1 => R1) {

    def as_ext[T1, G1](columnName: String)(implicit shape1: Shape[_ <: FlatShapeLevel, R1, T1, G1]) = {
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

  implicit class miaolegemiRepExtensionMethod1111[R1](repLike: R1) {

    def as[T1, G1](columnName: String)(implicit shape1: Shape[_ <: FlatShapeLevel, R1, T1, G1]): SqlRep[Any] = ???

  }

}