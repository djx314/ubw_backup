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
        val q"""(..${tablesProvide}) => {$body}""" = s
        val transformerGen = (oldName: String, newName: String) => new Transformer {
          override def transform(tree: Tree): Tree = {
            tree match {
              case ValDef(mods, TermName(`oldName`), tTree, valueDef) => ValDef(mods, TermName(newName), tTree, valueDef)
              case Ident(TermName(`oldName`)) => Ident(TermName(newName))
              case Bind(TermName(`oldName`), typeBind) => Bind(TermName(newName), typeBind)
              case other => super.transform(other)
            }
          }
        }

        val paramsq: List[(String, Tree)] = tablesProvide.map { case q"""$mods val ${ TermName(paramName) } : $paramType = ${_}""" => {
          paramName -> paramType
        } }

        val qqqqqq11 = paramsq.map { case (vName, vType) => {
          Bind(TermName(vName), Typed(Ident(termNames.WILDCARD), vType))
        } }

        val convert = (contentTree: Tree) => {
          val bbbb = q"""{ case (..$qqqqqq11) => $contentTree }"""
          val nameConvert = paramsq.foldLeft(bbbb: Tree) { case (baseFunction, (paramName, _)) => {
            val nameTranformer = transformerGen(paramName, c.freshName(paramName))
            nameTranformer.transform(baseFunction)
          } }
          nameConvert
        }

        val functionTransformer = new Transformer {
          override def transform(tree: Tree): Tree = {
            tree match {
              case q"""${x1}.mlgb[${x2}](${x3})(${x4})""" =>
                val nameConvert = convert(x3)
                val aa = q"""$x1.where { $nameConvert }"""
                this.transform((aa))

              case q"""${x1}.mlgb_orderby[${x2}](${x3})(${x4})""" =>
                val nameConvert = convert(x3)
                val aa = q"""$x1.order_by { $nameConvert }"""
                this.transform((aa))

              case q"""select.apply[${_}](..$columns)""" =>
                val newColumns = (for {
                  column <- columns
                } yield {
                  println(column + "11" * 100)
                  val q"""miaolegemiRepExtensionMethod1111[..${_}](..${ hahahaha :: Nil }).hhhh[..${_}](..${ columnName :: Nil })(..${_})""" = column
                  val valToMatch = (body: Tree) => {
                    val name = TermName(c.freshName)
                    val types = tq"""(..${paramsq.map(_._2)})"""
                    q"""
                       ($name : $types) => $name match {
                        case (..$qqqqqq11) => $body
                       }
                     """
                  }
                  val nameConvert = paramsq.foldLeft(valToMatch(hahahaha): Tree) { case (baseFunction, (paramName, _)) => {
                    val nameTranformer = transformerGen(paramName, c.freshName(paramName))
                    nameTranformer.transform(baseFunction)
                  } }
                  q"""${nameConvert}.as($columnName)"""
                })
                q"""select(..$newColumns)"""

              case other => {
                super.transform(other)
              }
            }
          }
        }
        val aa = functionTransformer.transform(c.untypecheck(body))
        val bbb = {
          val vName = TermName(c.freshName)
          val forName = TermName(c.freshName)
          val valTypeMap = paramsq.map { case (key, resultType) => TermName(c.freshName) -> resultType}
          val forQuery = valTypeMap.map { case (valName, resultType) => fq"""$valName <- TableQuery[$resultType]""" }
          q"""
            {
              val $vName = { $aa }
              val $forName = for(..$forQuery) yield {(..${valTypeMap.map(_._1)})}
              $vName.queryResult($forName)
            }
           """
        }
        println(bbb)
        bbb
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