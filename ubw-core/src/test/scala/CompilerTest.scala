package org.xarcher.ubw.core

import java.sql.{DriverManager, Timestamp}

import org.joda.time.DateTime
import play.api.libs.json._
import scala.annotation.tailrec
import scalaz._, Scalaz._
import scala.language.higherKinds
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions
import org.xarcher.ubw.core.UbwPgDriver.api._
import org.scalatest._
import org.scalatest.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration

class CompilerTest extends FlatSpec
with ScalaFutures
with Matchers
with BeforeAndAfter
with OneInstancePerTest {

  val db = Database.forURL(
    url = "jdbc:postgresql://192.168.1.110:5432/ubw?user=postgres",
    driver = "org.postgresql.Driver",
    user = "postgres",
    password = "postgres"
  )

  val slickCompiler = new SlickCompiler {}
  val tableQuerys = slickCompiler.tNameMap.toList.map(_._2)
  val schemas = tableQuerys.tail.foldLeft(tableQuerys.head.schema)((s, t) => {
    s ++ t.schema
  })

  before {
    Await.result(db.run(schemas.create), Duration.Inf)
  }

  after {
    Await.result(db.run(schemas.drop), Duration.Inf)
  }


  /*"aaaa" should "bbbb" in {
    db.run(slickCompiler.run.result).map(s => println(s.mkString("\n11\n")))
  }*/

  "cccc" should "ddddd" in {
    db.run(slickCompiler.run1111).map(s => println(s.mkString("\n11\n")))
  }

}