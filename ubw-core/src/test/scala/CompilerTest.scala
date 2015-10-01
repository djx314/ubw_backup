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

  def genTq(tName: String) = {
    object tableQuery extends TableQuery(cons => new UbwTable(cons, tName))
    tName -> tableQuery
  }

  //key 和 tableQuery 之间的 map
  val tNameMap: Map[String, TableQuery[UbwTable]] = Map(
    genTq("abc"),
    genTq("bcd")
  )

  //kay 和 query 的对应信息
  val tQueryMap: List[(String, Query[Rep[JsValue], JsValue, Seq])] =
    tNameMap.map { case (key, tableQuery) => {
      key -> tableQuery.map(_.data)
    } }.toList

  //query 的 key 和列操作的映射关系信息
  val queryMap = List(
    UColumn("abc", column => column +> "bbb"),
    UColumn("bcd", column => column +> "bcd"),
    UColumn("abc", column => column +> "ddd" +> "eeee")
  )

  val slickCompiler = new SlickCompiler {}

  //翻译 list 信息到 query
  def run(implicit ec: ExecutionContext): DBIO[Seq[Seq[JsValue]]] = {
    slickCompiler.xiaomai(tQueryMap, queryMap)
  }

  val tableQuerys = tNameMap.toList.map(_._2)
  val schemas = tableQuerys.tail.foldLeft(tableQuerys.head.schema)((s, t) => {
    s ++ t.schema
  })

  before {
    val json1 = Json.parse("""{ "aaa": "我是萌萌哒的aaaaa", "bbb": 2333, "ddd": { "eeee": "我是深层的eeee", "fff": "gherhrjyukuiiu" } }""")
    val json2 = Json.parse("""{ "aaa": "我是萌萌哒的第二个aaaaa", "bbb": "我是萌萌哒的第二个bbbb", "ddd": { "eeee": "我是第二个深层的eeee", "fff": "gherhrjyukuiiu" } }""")
    val bcdJson = Json.parse("""{ "bcd": "我是第二个表的数据", "bbb": "aaaa", "ddd": { "eee": "sdfsgferhrthj", "fff": "gherhrjyukuiiu" } }""")
    val bcd2Json = Json.parse("""{ "bcd": 1234, "bbb": "aaaa", "ddd": { "eee": "sdfsgferhrthj", "fff": "gherhrjyukuiiu" } }""")
    val action = schemas.create >> {
      tNameMap("abc") ++= List(json1, json2).map(s => Ubw(data = s))
    } >> {
      tNameMap("bcd") ++= List(bcdJson, bcd2Json).map(s => Ubw(data = s))
    }
    Await.result(db.run(action), Duration.Inf)
  }

  after {
    Await.result(db.run(schemas.drop), Duration.Inf)
  }

  "cccc" should "ddddd" in {
    Await.result(db.run(run).map(s => println(s)), Duration.Inf)
  }

}