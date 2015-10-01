package org.xarcher.ubw.core

import java.sql.Timestamp

import com.github.tminglei.slickpg._
import org.joda.time.DateTime
import play.api.db.slick.HasDatabaseConfig
import play.api.libs.json._
import scalaz._, Scalaz._
import scala.language.higherKinds
import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

trait UbwPgDriver extends slick.driver.PostgresDriver
with PgArraySupport
with PgDateSupport
with PgRangeSupport
with PgHStoreSupport
with PgPlayJsonSupport
with PgSearchSupport
with PgPostGISSupport
with array.PgArrayJdbcTypes {
  driver =>

  override val pgjson = "jsonb"

  object pgAPI extends API with SimpleQLPlus {

    implicit val strListTypeMapper: DriverJdbcType[List[String]] = new SimpleArrayJdbcType[String]("text").to(_.toList)

    implicit val playJsonArrayTypeMapper: DriverJdbcType[List[JsValue]] =
      new AdvancedArrayJdbcType[JsValue](pgjson,
        (s) => utils.SimpleArrayUtils.fromString[JsValue](Json.parse(_))(s).orNull,
        (v) => utils.SimpleArrayUtils.mkString[JsValue](_.toString())(v)
      ).to(_.toList)

    implicit val jdateColumnType =
      MappedColumnType.base[DateTime, Timestamp](
        dt => new Timestamp(dt.getMillis),
        ts => new DateTime(ts.getTime)
      )

  }

  override val api = pgAPI

  trait ImplicitsPlus extends ArrayImplicits
  with DateTimeImplicits
  with RangeImplicits
  with HStoreImplicits
  with JsonImplicits
  with SearchImplicits
  with PostGISImplicits

  trait SimpleQLPlus extends ImplicitsPlus
  with SearchAssistants
  with PostGISAssistants

  val plainAPI = new API with PlayJsonPlainImplicits

}

object UbwPgDriver extends UbwPgDriver

trait DBBase {

  val ubwDriver: UbwPgDriver

  import ubwDriver.api._

  implicit class QueryExtendClass[A, B, C[_]](query: Query[A, B, C]) {

    type QueryType = Query[A, B, C]

    def queryIf(bool: => Boolean)(ifQuery: QueryType => QueryType): QueryType =
      if (bool) ifQuery(query) else query

  }

  implicit def dbioActionMonad(implicit ec: ExecutionContext) =
    new Monad[DBIO] {
      def point[A](a: => A) = DBIO.successful(a)

      def bind[A, B](fa: DBIO[A])(f: (A) => DBIO[B]): DBIO[B] = fa flatMap f
    }

}
