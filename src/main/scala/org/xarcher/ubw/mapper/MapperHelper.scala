package net.scalax.ubw.mapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._
import org.xarcher.cpoi.{CellData, WriteableCellOperationAbs}
import slick.ast.TypedType

import scala.concurrent.ExecutionContext
import scala.language.higherKinds
import scala.language.implicitConversions
import scala.reflect.runtime.universe._
import slick.dbio._
import slick.driver.{JdbcProfile, JdbcActionComponent}
import slick.lifted._

trait MapperHelper {

  implicit class FilterIfNeedHelper[P1](rep1: Rep[P1]) {

    @inline def &&&[P2, R](need: Boolean, rep2: Rep[P2])
      (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R], convert: Rep[P1] => Rep[R]): Rep[R] = {
      if (need)
        new BooleanColumnExtensionMethods(rep1).&&(rep2)
      else
        convert(rep1)
    }

    @inline def |||[P2, R](need: Boolean, rep2: Rep[P2])
      (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R], convert: Rep[P1] => Rep[R]): Rep[R] =
      if (need)
        new BooleanColumnExtensionMethods(rep1).||(rep2)
      else
        convert(rep1)

    @inline def &&&[P2, R](rep2: Rep[P2])
                          (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R]): Rep[R] = {
      new BooleanColumnExtensionMethods(rep1).&&(rep2)
    }

    @inline def |||[P2, R](rep2: Rep[P2])
                          (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R]): Rep[R] =
      new BooleanColumnExtensionMethods(rep1).||(rep2)

  }

  implicit class FilterIfNeedRepHelper[P1](repContent1: (Boolean, Rep[P1])) {

    @inline def &&&[P2, R](rep2: Rep[P2])
      (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R], convert: Rep[R] => Rep[P2]): Rep[P2] = {
      val (need, rep1) = repContent1
      if (need)
        convert(new BooleanColumnExtensionMethods(rep1).&&(rep2))
      else
        rep2
    }

    @inline def |||[P2, R](rep2: Rep[P2])
      (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R], convert: Rep[R] => Rep[P2]): Rep[P2] = {
      val (need, rep1) = repContent1
      if (need)
        convert(new BooleanColumnExtensionMethods(rep1).||(rep2))
      else
        rep2
    }

  }

  trait NeedFilter {

    def &&[P1, P2, R](need: Boolean, rep1: Rep[P1], rep2: Rep[P2])
    (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R])
    : Rep[R] = {
      new BooleanColumnExtensionMethods(rep1).&&(rep2)
    }

    def ||[P1, P2, R](need: Boolean, rep1: Rep[P1], rep2: Rep[P2])
    (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R])
    : Rep[R] = {
      new BooleanColumnExtensionMethods(rep1).||(rep2)
    }

    def unary_![P1](need: Boolean, rep1: Rep[P1]): Rep[P1] = {
      new BooleanColumnExtensionMethods(rep1).unary_!
    }

  }

}

object MapperHelper extends MapperHelper