package org.xarcher.ubw.hlist

import org.scalatest._
import org.scalatest.concurrent._
import org.slf4j.LoggerFactory
import shapeless._

import scala.beans.BeanProperty
import scala.language.higherKinds

/**
  * Created by djx314 on 15-6-22.
  */

class HMapTest extends FlatSpec
with ScalaFutures
with Matchers
with BeforeAndAfter
with OneInstancePerTest {

  type TT = String :: Int :: HNil
  val cc = "bac"
  val aa = new HMapGen[TT] {
    @HMapKeyInfo("2333", 6)
    @HMapKeyInfo(cc, 7)
    val content = "2333" :: 666 :: HNil
  }

  "Small table" should "update some colunms" in {
    val bb = DecodeHMap.decodePrintln(
      new HMapGen[TT] {
        @HMapKeyInfo("2333", 6)
        @HMapKeyInfo(cc, 7)
        val content = "2333" :: 666 :: HNil
      }
    )
    println(bb)
  }

}