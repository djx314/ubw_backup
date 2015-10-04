package org.xarcher.ubw.core.parse

import org.parboiled2._

import org.xarcher.ubw.core.slick.UbwPgDriver.api._
import play.api.libs.json.JsValue
import shapeless._

class ca1111111111111111111111111111(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Int] = rule {
    Term ~ zeroOrMore(
      '+' ~ Term ~> ((_: Int) + _)
        | '-' ~ Term ~> ((_: Int) - _))
  }

  def Term = rule {
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> ((_: Int) * _)
        | '/' ~ Factor ~> ((_: Int) / _))
  }

  def Factor = rule { Number | Parens }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Number: Rule[HNil, Int :: HNil] = rule { capture(Digits) ~> ((s: String) => s.toInt)/*(_.toInt)*/}

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}

//new Calculator("1+1").InputLine.run() // evaluates to `scala.util.Success(2)`

class JsValueRepParser(val input: ParserInput, val jsValueRep: Rep[Option[JsValue]]) extends Parser with StringBuilding {

  private val `TEXTDATA-BASE` = CharPredicate.Printable -- '"'
  private val QTEXTDATA = `TEXTDATA-BASE` ++ "\r\n"
  val TEXTDATA = `TEXTDATA-BASE`//-- fieldDelimiter

  def field = rule { `quoted-field` | `unquoted-field` }

  def `quoted-field` = rule {
    OWS ~ '"' ~ clearSB() ~ zeroOrMore((QTEXTDATA | "\"\"") ~ appendSB()) ~ '"' ~ OWS ~ push(sb.toString)
  }

  def `unquoted-field` = rule { capture(zeroOrMore(TEXTDATA)) }

  def OWS = rule { zeroOrMore(' ') }

  //val getField: String => Rep[Option[JsValue]] =

  def `column-key`: Rule[HNil, Rep[Option[JsValue]] :: HNil] = rule { field ~> ((s: String) => jsValueRep +> s) }

  //def `js-value` = rule { `column-key` ~ OWS ~ "->" ~ OWS ~ field ~> ((mi: Rep[Option[JsValue]]) => (str: String) => (mi +> str)) }

  /*def Factor = rule { `js-value` | Parens }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Expression: Rule1[Rep[Option[JsValue]]] = rule {
    Term ~ zeroOrMore(
      '+' ~ Term ~> ((_: Int) + _)
        | '-' ~ Term ~> ((_: Int) - _))
  }

  def Term = rule {
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> ((_: Int) * _)
        | '/' ~ Factor ~> ((_: Int) / _))
  }*/

}