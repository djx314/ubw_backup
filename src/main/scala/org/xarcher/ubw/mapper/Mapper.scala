package org.xarcher.ubw.mapper

import io.circe._, io.circe.generic.auto._, io.circe.syntax._
import org.xarcher.cpoi.{CellData, WriteableCellOperationAbs}
import org.xarcher.ubw.wrapper.{QueryInfo, SqlWrapper, SqlRep}
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

    trait SqlWrapperHelper[E] {
      val sqlWrapper: SqlWrapper[E]
      val query: Query[E, _, Seq]
      def result
      (implicit
       ec: ExecutionContext,
       ev: Query[_, sqlWrapper.repGens.ValType, Seq] => JdbcActionComponent#StreamingQueryActionExtensionMethods[Seq[sqlWrapper.repGens.ValType], sqlWrapper.repGens.ValType],
       av: Rep[Int] => JdbcProfile#QueryActionExtensionMethods[Int, NoStream]
      ): QueryInfo = {
        sqlWrapper.queryResult(query)(ec, ev, av)
      }
    }

    def by(columns: SqlRep[E, _, _, _]*): SqlWrapperHelper[E] = {
      val sqlWrapper1 = SqlWrapper(
        select = columns.toList
      )
      new SqlWrapperHelper[E] {
        override val sqlWrapper = sqlWrapper1
        override val query = query1
      }
    }

  }

}

object Mapper extends Mapper