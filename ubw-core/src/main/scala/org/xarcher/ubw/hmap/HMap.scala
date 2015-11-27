package org.xarcher.ubw.hlist

import scala.annotation.StaticAnnotation
import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros
/**
  * Created by djx314 on 15-5-24.
  */
trait MacroUtils {

  val c: Context
  import c.universe._

  def typeFromParamTree(tree: Tree) = c.typecheck(tree.duplicate, c.TYPEmode).tpe

  def extractTermName(methodSymbol: Name) = {
    val TermName(s) = methodSymbol
    s
  }

}

trait JpaJavaModels extends MacroUtils {

  val c: Context
  import c.universe._

}