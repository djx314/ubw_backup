package org.xarcher.ubw.slick

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
  * Created by djx314 on 15-5-16.
  */
object SelectMacro {

  def decodePrintln(obj: Any): Any = macro SelectMacroImpl.impl

}

object UbwMacro {
  def body(obj: Any): Any = macro UbwMacroImpl.impl
}

class UbwMacroImpl(override val c: Context) extends MacroUtils {

  import c.universe._

  def impl(obj: c.Expr[Any]): c.Expr[Any] = {
    val resultTree = obj match {
      case Expr(s) =>
        val q"""(..${tablesProvide}) => {
          $body
        }""" = s
        val transformerGen = new Transformer {
          override def transform(tree: Tree): Tree = {
            tree match {
              case q"""${x1}.mlgb[${x2}](${x3})(${x4})""" =>
                println(x1.toString + "\n" + x2.toString + "\n" + "2333" + "\n")
                q"""${x1}.mlgb[${x2}](${x3})(${x4})"""
              case other => {
                //println(other + "2333")
                super.transform(other)
              }
            }
          }
        }
        //println(body)
        //val q"""${_}.mlgb[${_}](${h})(${t})""" = body
        //println(h)
        //println(t)
        (transformerGen.transform(body))
        q"""2333"""
      case _ => c.abort(c.enclosingPosition, "请输入一个代码块")
    }
    c.Expr(resultTree)
  }

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
            ${ s }
          }
         """
      //println(aa)
      c.Expr(transformerGen("bbbb", newFreshName).transform(c.untypecheck(aa)))
    /*case decl => {
      c.abort(c.enclosingPosition, "Underlying class must not be top-level and without companion")
    }*/
  }

}