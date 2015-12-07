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
      val kk = TermName("bbbb").decodedName.toTermName
      //val q"""scala.this.Predef.println($bbbbName)""" = s
      println(s)
      val q"""val ${abcd}: String = "bbbbbbbb"""" = q"""val bbbb: String = "bbbbbbbb""""
      val q"""scala.Predef.println(${efgh})""" = q"""scala.Predef.println(bbbb)"""
      //println(bbbbName.getClass.toString)
      println(abcd)
      println(abcd.getClass.toString)
      println(kk.getClass.toString)
      println(abcd == kk)
      println(efgh)
      println(efgh.getClass.toString)

      val aa =
        q"""
          {
            {
              val kkkk: String = "2333" * 100;
              val ${abcd}: String = "5678" * 100;
              {
                println(${efgh})
                ..${s}
              }
            }
            666
          }
         """
      println(aa)
      c.Expr(aa)
    /*case decl => {
      c.abort(c.enclosingPosition, "Underlying class must not be top-level and without companion")
    }*/
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