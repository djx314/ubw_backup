package org.xarcher.ubw.core.parse

import org.parboiled2
import org.parboiled2._

import org.xarcher.ubw.core.slick.UbwPgDriver.api._
import play.api.libs.json.JsValue
import shapeless._

class ca1111111111111111111111111111(val input: ParserInput) extends Parser {
  def InputLine = rule { Expression ~ EOI }

  def Expression: Rule1[Int] = rule {
    Term ~ zeroOrMore(
      '+' ~ Term ~> ((_: Int) + _)
        | '-' ~ Term ~> ((_: Int) - _)
    )
  }

  def Term = rule {
    Factor ~ zeroOrMore(
      '*' ~ Factor ~> ((_: Int) * _)
        | '/' ~ Factor ~> ((_: Int) / _)
    )
  }

  def Factor = rule { Number | Parens }

  def Parens = rule { '(' ~ Expression ~ ')' }

  def Number: Rule[HNil, Int :: HNil] = rule { capture(Digits) ~> ((s: String) => s.toInt)/*(_.toInt)*/}

  def Digits = rule { oneOrMore(CharPredicate.Digit) }
}

//new Calculator("1+1").InputLine.run() // evaluates to `scala.util.Success(2)`

class JsValueRepParser(val input: ParserInput, val jsValueRep: Rep[Option[JsValue]]) extends Parser with StringBuilding {

  //def InputLine: Rule[HNil, Rep[Option[JsValue]] :: HNil] = rule { `column-key` ~ EOI }

  private val `TEXTDATA-BASE` = CharPredicate.Visible -- ' '
  val TEXTDATA = `TEXTDATA-BASE`//-- fieldDelimiter

  //def field = zeroOrMore(`unquoted-field`)

  /*def `quoted-field` = rule {
    OWS ~ '"' ~ clearSB() ~ zeroOrMore((QTEXTDATA | "\"\"") ~ appendSB()) ~ '"' ~ OWS ~ push(sb.toString)
  }*/

  def `unquoted-field`: Rule[HNil, HNil] = rule { OWS ~ TEXTDATA ~ OWS }

  def OWS: Rule[HNil, HNil] = rule { zeroOrMore(' ') }

  /*def `column-key`: Rule[HNil, Rep[Option[JsValue]] :: HNil] = rule { field ~> ((s: String) => {
    println(s)
    jsValueRep +> s
  }) }*/

  //def `js-value`/*: Rule1[Rep[Option[JsValue]]]*/ = rule { `column-key` ~ OWS ~ "->" ~ OWS ~ field ~> ((mi: Rep[Option[JsValue]], str: String) => mi +> str) }

  /*def Factor: Rule1[Rep[Option[JsValue]]] = rule { `js-value` | Parens }

  def Parens: Rule1[Rep[Option[JsValue]]] = rule { '(' ~ Expression ~ ')' }

  type aabb = Rule[Rep[Option[JsValue]] :: HNil, Rep[Option[JsValue]] :: HNil]
  def Expression: Rule1[Rep[Option[JsValue]]] = rule {
    Term ~ zeroOrMore(
      OWS ~ '+' ~ OWS ~ `column-key` ~> ((mi: Rep[Option[JsValue]], str: Rep[Option[JsValue]]) => (mi.asColumnOf[Option[BigDecimal]] + str.asColumnOf[Option[BigDecimal]]).asColumnOf[Option[JsValue]])
      | OWS ~ '-' ~ OWS ~ `column-key` ~> ((mi: Rep[Option[JsValue]], str: Rep[Option[JsValue]]) => (mi.asColumnOf[Option[BigDecimal]] - str.asColumnOf[Option[BigDecimal]]).asColumnOf[Option[JsValue]])
    )
  }

  def Term: Rule1[Rep[Option[JsValue]]] = rule {
    Factor ~ zeroOrMore(
      OWS ~ '*' ~ OWS ~ `column-key` ~> ((mi: Rep[Option[JsValue]], str: Rep[Option[JsValue]]) => (mi.asColumnOf[Option[BigDecimal]] * str.asColumnOf[Option[BigDecimal]]).asColumnOf[Option[JsValue]])
      | OWS ~ '/' ~ OWS ~ `column-key` ~> ((mi: Rep[Option[JsValue]], str: Rep[Option[JsValue]]) => (mi.asColumnOf[Option[BigDecimal]] / str.asColumnOf[Option[BigDecimal]]).asColumnOf[Option[JsValue]])
    )
  }*/

}