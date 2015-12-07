package org.xarcher.ubw.slick

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
  * Created by djx314 on 15-5-16.
  */
object SelectMacro {

  def decodePrintln(obj: Any): Any = macro SelectMacroImpl.impl

}

class SelectMacroImpl(override val c: Context) extends MacroUtils {

  import c.universe._

  def impl(obj: c.Expr[Any]): c.Expr[Any] = obj.tree match {
    case classDecl: ClassDef => {
      c.abort(c.enclosingPosition, "喵了个咪，不要 class 定义")
    }
    case s =>
      val transformerGen = (oldName: String, newName: String) => new Transformer {
        override def transform(tree: Tree): Tree = {
          tree match {
            case ValDef(mods, TermName(`oldName`), tTree, valueDef) => ValDef(mods, TermName(newName), tTree, valueDef)
            case Ident(TermName(`oldName`)) => Ident(TermName(newName))
            case other => super.transform(other)
          }
        }
      }

      val newFreshName = c.freshName
      val aa =
        q"""
          {
            val ${ TermName(newFreshName) } = 12
            ${ transformerGen("bbbb", newFreshName).transform(c.untypecheck(s)) }
          }
         """
      println(aa)
      c.Expr(aa)
    /*case decl => {
      c.abort(c.enclosingPosition, "Underlying class must not be top-level and without companion")
    }*/
  }

}