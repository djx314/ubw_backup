package net.scalax.ubw.helper

import scala.language.implicitConversions
import slick.ast.TypedType
import slick.lifted._

case class RichBRep[P](rep: Option[Rep[P]]) {

  def &&&[P2, R](rRep2: RichBRep[P2])
                (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    (rep -> rRep2.rep) match {
      case (Some(subRep1), Some(subRep2)) =>
        RichBRep(Option(new BooleanColumnExtensionMethods(subRep1).&&(subRep2)(om)))
      case (None, Some(subRep2)) =>
        RichBRep(Option(convertP2(subRep2)))
      case (Some(subRep1), None) =>
        RichBRep(Option(convertP(subRep1)))
      case _ =>
        RichBRep(None)
    }
  }

  def |||[P2, R](rRep2: RichBRep[P2])
                (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    (rep -> rRep2.rep) match {
      case (Some(subRep1), Some(subRep2)) =>
        RichBRep(Option(new BooleanColumnExtensionMethods(subRep1).||(subRep2)(om)))
      case (None, Some(subRep2)) =>
        RichBRep(Option(convertP2(subRep2)))
      case (Some(subRep1), None) =>
        RichBRep(Option(convertP(subRep1)))
      case _ =>
        RichBRep(None)
    }
  }

  def &&&[P2, R](when: Boolean, rRep2: => Rep[P2])
                (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    if (when) {
      &&&(RichBRep(Option(rRep2)))
    } else {
      &&&(RichBRep.empty[P2])
    }
  }

  def |||[P2, R](when: Boolean, rRep2: => Rep[P2])
                (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    if (when) {
      |||(RichBRep(Option(rRep2)))
    } else {
      |||(RichBRep.empty[P2])
    }
  }

  def &&&[P2, R](rRep2: Rep[P2])
                (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    &&&(RichBRep(Option(rRep2)))
  }

  def |||[P2, R](rRep2: Rep[P2])
                (implicit om: OptionMapperDSL.arg[Boolean, P]#arg[Boolean, P2]#to[Boolean, R], convertP: Rep[P] => Rep[R], convertP2: Rep[P2] => Rep[R]): RichBRep[R] = {
    |||(RichBRep(Option(rRep2)))
  }

  def ||=>[P2](rRep2: Rep[P2])
                 (implicit convertP: Rep[P] => Rep[Option[Boolean]], convertP2: Rep[P2] => Rep[Option[Boolean]]): Rep[Option[Boolean]] = {
    rep.map(convertP(_)).getOrElse(convertP2(rRep2))
  }

  def result(implicit tt: TypedType[Boolean], convert: Rep[P] => Rep[Option[Boolean]], columnMethods: Rep[Boolean] => BaseColumnExtensionMethods[Boolean]): Rep[Option[Boolean]] = {
    ||=>(LiteralColumn(true)(tt))(convert, (s: Rep[Boolean]) => columnMethods(s).?)
  }

}

object RichBRep {
  def empty[P] = RichBRep(Option.empty[Rep[P]])
}

trait BooleanRepHelper {

  implicit def booleanRepToRichRep[T](rep: Rep[T]): RichBRep[T] = RichBRep(Option(rep))
  implicit def booleanNeedRepToRichRep[T](plus: (Boolean, Rep[T])): RichBRep[T] = if (plus._1) {
    RichBRep(Option(plus._2))
  } else {
    RichBRep.empty[T]
  }
  implicit def booleanToOptionBooleanRep[T](baseRep: Rep[Boolean])(implicit columnMethods: Rep[Boolean] => BaseColumnExtensionMethods[Boolean]): Rep[Option[Boolean]] = columnMethods(baseRep).?

  /*implicit class FilterIfNeedHelper[P1](rep1: Rep[P1]) {

    @inline def &&&[P2, R](need: Boolean, rep2: Rep[P2])
      (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R], convert: Rep[P1] => Rep[R]): Rep[R] = {
      if (need)
        new BooleanColumnExtensionMethods(rep1).&&(rep2)
      else
        convert(rep1)
    }

    @inline def |||[P2, R](need: Boolean, rep2: Rep[P2])
      (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R], convert: Rep[P1] => Rep[R]): Rep[R] =
      if (need)
        new BooleanColumnExtensionMethods(rep1).||(rep2)
      else
        convert(rep1)

    @inline def &&&[P2, R](rep2: Rep[P2])
                          (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R]): Rep[R] = {
      new BooleanColumnExtensionMethods(rep1).&&(rep2)
    }

    @inline def |||[P2, R](rep2: Rep[P2])
                          (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R]): Rep[R] =
      new BooleanColumnExtensionMethods(rep1).||(rep2)

  }

  implicit class FilterIfNeedRepHelper[P1](repContent1: (Boolean, Rep[P1])) {

    @inline def &&&[P2, R](rep2: Rep[P2])
      (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R], convert: Rep[R] => Rep[P2]): Rep[P2] = {
      val (need, rep1) = repContent1
      if (need)
        convert(new BooleanColumnExtensionMethods(rep1).&&(rep2))
      else
        rep2
    }

    @inline def |||[P2, R](rep2: Rep[P2])
      (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R], convert: Rep[R] => Rep[P2]): Rep[P2] = {
      val (need, rep1) = repContent1
      if (need)
        convert(new BooleanColumnExtensionMethods(rep1).||(rep2))
      else
        rep2
    }

  }

  trait NeedFilter {

    def &&[P1, P2, R](need: Boolean, rep1: Rep[P1], rep2: Rep[P2])
    (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R])
    : Rep[R] = {
      new BooleanColumnExtensionMethods(rep1).&&(rep2)
    }

    def ||[P1, P2, R](need: Boolean, rep1: Rep[P1], rep2: Rep[P2])
    (implicit om: OptionMapperDSL.arg[Boolean, P1]#arg[Boolean, P2]#to[Boolean, R])
    : Rep[R] = {
      new BooleanColumnExtensionMethods(rep1).||(rep2)
    }

    def unary_![P1](need: Boolean, rep1: Rep[P1]): Rep[P1] = {
      new BooleanColumnExtensionMethods(rep1).unary_!
    }

  }*/

}

object BooleanRepHelper extends BooleanRepHelper