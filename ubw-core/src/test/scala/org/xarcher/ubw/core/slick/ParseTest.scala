package org.xarcher.ubw.core.slick

import java.sql.{DriverManager, Timestamp}

import org.joda.time.DateTime
import org.xarcher.ubw.core.parse.JsValueRepParser
import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import org.xarcher.ubw.core.slick.UbwPgDriver.api._
import org.scalatest._
import org.scalatest.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.existentials
import scala.util.{Failure, Success}

object ParseTest extends App {

  val db = Database.forURL(
    url = "jdbc:postgresql://127.0.0.1:5432/ubw?user=postgres",
    driver = "org.postgresql.Driver",
    user = "postgres",
    password = "postgres"
  )

  val tableaa = new TableQuery(cons => new UbwTable(cons, "aaaabb"))

  Await.result(db.run(tableaa.schema.create), Duration.Inf)

  try {

    val aa = tableaa.map(ss => {
      new JsValueRepParser("      abcdefgh -> bcde  ->       abcde    ", ss.data.?).InputLine.run() match {
        case Success(s) => s
        case Failure(e) =>
          e.printStackTrace
          throw e
      }
    }).result

    Await.result(db.run(aa), Duration.Inf)
  } finally {
    Await.result(db.run(tableaa.schema.drop), Duration.Inf)
  }

}