package net.scalax.ubw.helper

import slick.lifted._

trait Alias {
  type SlickRange = net.scalax.ubw.mapper.SlickRange
  val SlickRange = net.scalax.ubw.mapper.SlickRange

  type SlickPage = net.scalax.ubw.mapper.SlickPage
  val SlickPage = net.scalax.ubw.mapper.SlickPage

  type ColumnOrder = net.scalax.ubw.mapper.ColumnOrder
  val ColumnOrder = net.scalax.ubw.mapper.ColumnOrder

  type SlickParam = net.scalax.ubw.mapper.SlickParam
  val SlickParam = net.scalax.ubw.mapper.SlickParam

  type DataGen = net.scalax.ubw.mapper.DataGen
  val DataGen = net.scalax.ubw.mapper.DataGen

  type ResultGen = net.scalax.ubw.mapper.ResultGen
  val ResultGen = net.scalax.ubw.mapper.ResultGen

  type PropertyInfo = net.scalax.ubw.mapper.PropertyInfo
  val PropertyInfo = net.scalax.ubw.mapper.PropertyInfo

  type QueryInfo = net.scalax.ubw.mapper.QueryInfo
  val QueryInfo = net.scalax.ubw.mapper.QueryInfo

  type TableData = net.scalax.ubw.mapper.TableData
  val TableData = net.scalax.ubw.mapper.TableData

}