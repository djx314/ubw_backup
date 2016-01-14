package org.xarcher.ubw.macros

import org.xarcher.ubw.wrapper.{SqlWrapper, QueryInfo}

import scala.reflect.macros.blackbox.Context
import scala.language.experimental.macros

/**
  * Created by djx314 on 15-5-16.
  */
object Ubw {
  def from(obj: Any): QueryInfo = macro UbwMacroImpl.impl
  def gfrom(obj: Any): QueryInfo = macro UbwGMacroImpl.impl
}

class UbwMacroImpl(override val c: Context) extends MacroUtils {

  import c.universe._

  val paramTransformGen = (oldName: String, newName: String) => new Transformer {
    override def transform(tree: Tree): Tree = {
      tree match {
        case ValDef(mods, TermName(`oldName`), tTree, valueDef) => ValDef(mods, TermName(newName), tTree, valueDef)
        case Ident(TermName(`oldName`)) => Ident(TermName(newName))
        case Bind(TermName(`oldName`), typeBind) => Bind(TermName(newName), typeBind)
        case other => super.transform(other)
      }
    }
  }

  def impl(obj: c.Expr[Any]): c.Expr[QueryInfo] = {
    val resultTree = obj match {
      case Expr(s) =>
        val q"""
          ..$ubwContentBody
          (..${tablesProvide}) => {$body}
        """ = s

        val tablesInfo: List[(String, Tree)] = for {
          q"""$mods val ${ TermName(paramName) } : $paramType = ${_}""" <- tablesProvide
        } yield
          paramName -> paramType

        val tableParams = tablesInfo.map { case (vName, vType) => {
          Bind(TermName(vName), Typed(Ident(termNames.WILDCARD), vType))
        } }

        val convert = (contentTree: Tree) => {
          val caseBody = q"""{ case (..$tableParams) => $contentTree }"""
          val nameConvert = tablesInfo.foldLeft(caseBody: Tree) { case (baseCase, (vName, _)) => {
            val nameTranformer = paramTransformGen(vName, c.freshName(vName))
            nameTranformer.transform(baseCase)
          } }
          nameConvert
        }

        val functionTransformer = new Transformer {
          override def transform(tree: Tree): Tree = {
            tree match {
              case q"""$x1.where[$x2]($x3)($x4)""" =>
                val nameConvert = convert(x3)
                val aa = q"""$x1.where_ext { $nameConvert }"""
                this.transform((aa))

              case q"""$x1.where_if[$x2]($needParam)($x3)($x4)""" =>
                val nameConvert = convert(x3)
                val aa = q"""$x1.where_if_ext($needParam) { $nameConvert }"""
                this.transform((aa))

              case q"""$x1.order_by[$x2]($x3)($x4)""" =>
                val nameConvert = convert(x3)
                val aa = q"""$x1.order_by_ext { $nameConvert }"""
                this.transform((aa))

              case q"""$x1.order_by_if[$x2]($needParam)($x3)($x4)""" =>
                val nameConvert = convert(x3)
                val aa = q"""$x1.order_by_if_ext($needParam) { $nameConvert }"""
                this.transform((aa))

              /*case q"""$x1.group_by[..$x2]($x3)($x4)""" =>
                val nameConvert = convert(x3)
                val aa = q"""$x1.group_by_ext { $nameConvert }"""
                this.transform((aa))*/

              case q"""org.xarcher.ubw.wrapper.select.apply[..${_}](..$columns)""" =>
                val newColumns = (for {
                  eachColumn <- columns
                } yield {
                    val columnTransaformer = new Transformer {
                      override def transform(tree: Tree): Tree = {
                        tree match {
                          case contentTree@q"""${_}[..${_}](..${ columnDescribe }).as[..${_}](..${ columnName :: Nil })(..${_})""" =>
                            val valToMatch: List[Tree] => Tree = (body) => {
                              val name = TermName(c.freshName)
                              val types = tq"""(..${tablesInfo.map(_._2)})"""
                              q"""
                              ($name : $types) => $name match {
                                case (..$tableParams) => {
                                  ..$body
                                }
                              }
                              """
                            }

                            val nameConvert = tablesInfo.foldLeft(valToMatch(columnDescribe)) { case (baseFunction, (paramName, _)) => {
                              val nameTranformer = paramTransformGen(paramName, c.freshName(paramName))
                              nameTranformer.transform(baseFunction)
                            } }

                            q"""${nameConvert}.as_ext($columnName)"""
                          /*case q"${orderContent}.order(..$params)" =>
                            val resultContent = this.transform(orderContent)
                            q"""${resultContent}.order_ext(..$params)"""*/
                          case other =>
                            super.transform(other)
                        }
                      }
                    }

                    columnTransaformer.transform(eachColumn)
                })

                //println(columns)
                //println(newColumns)
                q"""org.xarcher.ubw.wrapper.select(..$newColumns)"""

              case other => {
                super.transform(other)
              }
            }
          }
        }
        val sqlWrapperBody = functionTransformer.transform(c.untypecheck(body))
        val dbioBody = {
          val vName = TermName(c.freshName)
          val forName = TermName(c.freshName)
          val valTypeMap = tablesInfo.map { case (key, resultType) => TermName(c.freshName) -> resultType}
          val forQuery = valTypeMap.map { case (valName, resultType) => fq"""$valName <- TableQuery[$resultType]""" }
          q"""
          {
            ..$ubwContentBody
            val $vName = { $sqlWrapperBody }
            val $forName = for(..$forQuery) yield { (..${valTypeMap.map(_._1)}) }
            $vName.queryResult($forName)
          }
         """
        }
        dbioBody

      case _ => c.abort(c.enclosingPosition, "请输入一个合符要求的代码块")
    }
    c.Expr(resultTree)
  }

}