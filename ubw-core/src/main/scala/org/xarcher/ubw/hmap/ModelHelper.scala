package org.xarcher.ubw.hlist

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import shapeless._

/**
  * Created by djx314 on 15-5-16.
  */
object DecodeHMap {

  def decodePrintln[HType <: HList](obj: HMapGen[HType]): Any = macro DecodeHMapImpl.impl

}

class HMapKeyInfo[T](key: T, index: Int) extends StaticAnnotation

trait HMapGen[HListType <: HList] {

  val content: HListType

}

class DecodeHMapImpl(override val c: Context) extends JpaJavaModels {

  import c.universe._

  def impl(obj: c.Expr[Any]): c.Expr[Any] = obj match {
    case classDecl: ClassDef => {
      c.Expr(genCode(classDecl))
    }
    case Expr(s) =>
      getAnnotations(s)
      c.Expr(q"""{ println(${s}); 2; }""")
    case decl => {
      c.abort(c.enclosingPosition, "Underlying class must not be top-level and without companion")
    }
  }

  def getAnnotations(tree: Tree) = {
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
  }

}