package org.xarcher.ubw.slick

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

/**
  * Created by djx314 on 15-5-16.
  */
object SelectMacro {

  def decodePrintln(obj: Symbol): Any = macro SelectMacroImpl.impl

}

class SelectMacroImpl(override val c: Context) extends MacroUtils {

  import c.universe._

  def impl(obj: c.Expr[Any]): c.Expr[Any] = obj match {
    case classDecl: ClassDef => {
      c.abort(c.enclosingPosition, "喵了个咪，不要 class 定义")
    }
    case Expr(s) =>
      val q"""scala.Symbol.apply(${Literal(Constant(kk: String))})""" = s
      println(kk + "打印出来的")
      c.Expr(q"""{ println(${s}); 2; }""")
    case decl => {
      c.abort(c.enclosingPosition, "Underlying class must not be top-level and without companion")
    }
  }

  /*def getAnnotations(tree: Tree) = {
    val q"""{
        $mods class $tpname[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }
        new $tpname1()
      };
    """ = tree
    val pros: List[Tree] = stats
    val annotations = pros.map(_.symbol).find(s => {
      s.name.decodedName.toString.trim == "content"
    }).get.annotations
    println(annotations)
  }

  protected def genCode(classDef: ClassDef) = {
    q"""println("asb")"""
  }*/

}