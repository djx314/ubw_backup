package org.xarcher.ubw.core.slick

import java.sql.{DriverManager, Timestamp}

import org.joda.time.DateTime
import play.api.libs.json._
import scala.annotation.tailrec
import scalaz._, Scalaz._
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

class CompilerTest extends FlatSpec
with ScalaFutures
with Matchers
with BeforeAndAfter
with OneInstancePerTest {

  val db = Database.forURL(
    url = "jdbc:postgresql://127.0.0.1:5432/ubw?user=postgres",
    driver = "org.postgresql.Driver",
    user = "postgres",
    password = "postgres"
  )

  //kay 和 query 的对应信息
  val tQueryMap: List[(String, UQuery)] =
    List(
      "aaaa1" -> new UTableQuery("abc"),
      "aaaa2" -> new UTableQuery("bcd")
    )

  //query 的 key 和列操作的映射关系信息
  val queryMap = List(
    UColumn("喵了个咪", "bbb", "aaaa1"),
    UColumn("喵了个jb", "bcd", "aaaa2"),
    UColumn("喵了个大jb","ddd", "aaaa1")
  )

  //翻译 list 信息到 query
  def run(implicit ec: ExecutionContext): DBIO[Seq[Seq[UItem]]] = {
    new UContent {
      override val querys = List(
        "aaaa1" -> new UTableQuery("abc"),
        "aaaa2" -> new UTableQuery("bcd")
      )
      override val columns = List(
        UColumn("黑狗", "aaa", "aaaa1"),
        UColumn("喵了个咪", "bbb", "aaaa1"),
        UColumn("喵了个jb", "bcd", "aaaa2"),
        UColumn("喵了个大jb","ddd", "aaaa1")
      )
      override val converts = List(new UColumnGt(UColumn("xxxx", "bcd", "aaaa2"), 234))
    }.result
  }

  def runWithSub(implicit ec: ExecutionContext): DBIO[Seq[Seq[UItem]]] = {
    val subContent = new UContent {
      override val querys = List(
        "aaaa1" -> new UTableQuery("abc"),
        "aaaa2" -> new UTableQuery("bcd")
      )
      override val columns = List(
        UColumn("黑狗", """aaa""", "aaaa1"),//bbbcc -> sb - ( aaa -> sb ) - ( bbbcc -> sb - ( aaa -> sb ) )
        UColumn("喵了个咪", "bbb", "aaaa1"),
        UColumn("喵了个jb", "bcd", "aaaa2"),
        UColumn("喵了个大jb","ddd", "aaaa1")
      )
      override val converts = List(
        /*new ColumnGt(UColumn("xxxx", "bcd", "aaaa2"), 234),
        new SortBy(UColumn("xxxx", "bcd", "aaaa2"), Option(true))*/
      )
    }.toContent
    val parentQueryMap = List(
      UColumn("白猫", "黑狗", "啊哈哈哈哈"),
      UColumn("划船不用浆", "喵了个咪", "啊哈哈哈哈"),
      UColumn("全靠浪","喵了个jb", "啊哈哈哈哈"),
      UColumn("夏目友人帐","喵了个大jb", "啊哈哈哈哈")
    )
    new UContent {
      override val querys = List("啊哈哈哈哈" -> subContent)
      override val columns = parentQueryMap
      override val converts = List(
        {
          new UColumnGt(UColumn("xxxx", "黑狗", "啊哈哈哈哈"), 567)/*and
          new UColumnGt(UColumn("xxxx", "黑狗", "啊哈哈哈哈"), 678) or
          new UColumnGt(UColumn("xxxx", "黑狗", "啊哈哈哈哈"), 789)and
          new UColumnLike(UColumn("xxxx", "黑狗", "啊哈哈哈哈"), "6789%") and
          new UColumnLike(UColumn("xxxx", "喵了个咪", "啊哈哈哈哈"), "%我是萌萌哒的第二个%")*/
        },
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
    val json1 = Json.parse("""{ "aaa": 123456, "bbb": "我是萌萌哒", "ddd": { "eeee": "我是深层的eeee", "fff": "gherhrjyukuiiu" } }""")
    val json2 = Json.parse("""{ "aaa": 678910, "bbb": "我是萌萌哒的第二个bbbb" }""")
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