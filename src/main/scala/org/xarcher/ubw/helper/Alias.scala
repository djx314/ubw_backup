package org.xarcher.ubw.helper

import slick.lifted._

trait Alias {
  type SlickRange = org.xarcher.ubw.mapper.SlickRange
  val SlickRange = org.xarcher.ubw.mapper.SlickRange

  type SlickPage = org.xarcher.ubw.mapper.SlickPage
  val SlickPage = org.xarcher.ubw.mapper.SlickPage

  type ColumnOrder = org.xarcher.ubw.mapper.ColumnOrder
  val ColumnOrder = org.xarcher.ubw.mapper.ColumnOrder

  type SlickParam = org.xarcher.ubw.mapper.SlickParam
  val SlickParam = org.xarcher.ubw.mapper.SlickParam

  type DataGen = org.xarcher.ubw.mapper.DataGen
  val DataGen = org.xarcher.ubw.mapper.DataGen

  type ResultGen = org.xarcher.ubw.mapper.ResultGen
  val ResultGen = org.xarcher.ubw.mapper.ResultGen

  type PropertyInfo = org.xarcher.ubw.mapper.PropertyInfo
  val PropertyInfo = org.xarcher.ubw.mapper.PropertyInfo

  type QueryInfo = org.xarcher.ubw.mapper.QueryInfo
  val QueryInfo = org.xarcher.ubw.mapper.QueryInfo

  type TableData = org.xarcher.ubw.mapper.TableData
  val TableData = org.xarcher.ubw.mapper.TableData

}