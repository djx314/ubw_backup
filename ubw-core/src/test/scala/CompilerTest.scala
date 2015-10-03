package org.xarcher.ubw.core

import java.sql.{DriverManager, Timestamp}

import org.joda.time.DateTime
import play.api.libs.json._
import scala.annotation.tailrec
import scalaz._, Scalaz._
import scala.concurrent.ExecutionContext
import org.xarcher.ubw.core.UbwPgDriver.api._
import org.scalatest._
import org.scalatest.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Await
import scala.concurrent.duration.Duration

import scala.language.higherKinds
import scala.language.implicitConversions
import scala.language.existentials

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

  //kay 和 query 的对应信息
  val tQueryMap: List[(String, UContent)] =
    List(
      "aaaa1" -> new UTableQueryContent("abc"),
      "aaaa2" -> new UTableQueryContent("bcd")
    )

  //query 的 key 和列操作的映射关系信息
  val queryMap = List(
    UColumn("喵了个咪", "bbb", "aaaa1"),
    UColumn("喵了个jb", "bcd", "aaaa2"),
    UColumn("喵了个大jb","ddd", "aaaa1")
  )

  //翻译 list 信息到 query
  def run(implicit ec: ExecutionContext): DBIO[Seq[Seq[UItem]]] = {
    new UQuery {
      override val contents = tQueryMap
      override val columns = queryMap
      override val converts = List(new ColumnGt(UColumn("xxxx", "bcd", "aaaa2"), 234))
    }.result
  }

  def runWithSub(implicit ec: ExecutionContext): DBIO[Seq[Seq[UItem]]] = {
    val subContent = new UQuery {
      override val contents = tQueryMap
      override val columns = queryMap
      override val converts = List(
        new ColumnGt(UColumn("xxxx", "bcd", "aaaa2"), 234),
        new SortBy(UColumn("xxxx", "bcd", "aaaa2"), Option(true))
      )
    }.toContent
    val parentQueryMap = List(
      UColumn("划船不用浆", "喵了个jb", "啊哈哈哈哈"),
      UColumn("全靠浪","喵了个大jb", "啊哈哈哈哈")
    )
    new UQuery {
      override val contents = List("啊哈哈哈哈" -> subContent)
      override val columns = parentQueryMap
      override val converts = List(
        new ColumnGt(UColumn("xxxx", "喵了个咪", "啊哈哈哈哈"), 567),
        new SortBy(UColumn("xxxx", "喵了个咪", "啊哈哈哈哈"), None),
        new SortBy(UColumn("xxxx", "喵了个大jb", "啊哈哈哈哈"), Option(true))
      )
    }.result
  }

  val abcTable = new TableQuery(cons => new UbwTable(cons, "abc"))
  val bcdTable = new TableQuery(cons => new UbwTable(cons, "bcd"))
  val tableQuerys = tQueryMap.map(_._2.query)
  val schemas = abcTable.schema ++ bcdTable.schema

  before {
    val json1 = Json.parse("""{ "aaa": "我是萌萌哒的aaaaa", "bbb": 2333, "ddd": { "eeee": "我是深层的eeee", "fff": "gherhrjyukuiiu" } }""")
    val json2 = Json.parse("""{ "aaa": "我是萌萌哒的第二个aaaaa", "bbb": "我是萌萌哒的第二个bbbb" }""")
    val bcdJson = Json.parse("""{ "bcd": "我是第二个表的数据", "bbb": "aaaa", "ddd": { "eee": "sdfsgferhrthj", "fff": "gherhrjyukuiiu" } }""")
    val bcd2Json = Json.parse("""{ "bcd": 1234, "bbb": "aaaa", "ddd": { "eee": "sdfsgferhrthj", "fff": "gherhrjyukuiiu" } }""")
    val action = schemas.create >> {
      new TableQuery(cons => new UbwTable(cons, "abc")) ++= List(json1, json2).map(s => Ubw(data = s))
    } >> {
      new TableQuery(cons => new UbwTable(cons, "bcd")) ++= List(bcdJson, bcd2Json).map(s => Ubw(data = s))
    }
    Await.result(db.run(action), Duration.Inf)
  }

  after {
    Await.result(db.run(schemas.drop), Duration.Inf)
  }

  "cccc" should "ddddd" in {
    Await.result(db.run(run).map(s => println(s.mkString("\n"))), Duration.Inf)
  }

  "eeee" should "ffff" in {
    Await.result(db.run(runWithSub).map(s => println(s.mkString("\n"))), Duration.Inf)
  }

}