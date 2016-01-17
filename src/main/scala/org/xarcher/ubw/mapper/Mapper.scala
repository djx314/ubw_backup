package org.xarcher.ubw.mapper

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

  class FilterIfNeedHelper[P1](rep1: Rep[P1]) {

    def and[P2, R](need: Boolean, rep2: Rep[P2])
      (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R], convert: Rep[P1] => Rep[R]): Rep[R] = {
      if (need)
        new BooleanColumnExtensionMethods(rep1).&&(rep2)
      else
        convert(rep1)
    }

    def or[P2, R](need: Boolean, rep2: Rep[P2])
      (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R], convert: Rep[P1] => Rep[R]): Rep[R] =
      if (need)
        new BooleanColumnExtensionMethods(rep1).||(rep2)
      else
        convert(rep1)

  }

  implicit def filterIfNeedHelper[P1](rep1: Rep[P1]) = new FilterIfNeedHelper[P1](rep1)

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