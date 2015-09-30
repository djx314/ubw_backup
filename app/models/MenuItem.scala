package models

import play.api.mvc.{AnyContent, Request}
import scala.annotation.tailrec
import utils.UbwKeys.FileType

case class MenuItem(
  id: Option[Long],
  parent: Long,
  name: String,
  url: Option[String],
  childrenUrl: Option[String],
  fileType: String,
  canHaveChild: Boolean,
  order: Double
)

case class MiniMenu(name: String, url: Option[String], fileType: String, canHaveChild: Boolean, childrenUrl: Option[String] = None)
case class CaseMiniMenu(miniMenu: MiniMenu, permissions: List[String], children: List[CaseMiniMenu] = Nil)
case class CaseMenuItem(menuItem: MenuItem, permissions: List[String])

case class MenuItemUtil()(implicit request: Request[AnyContent]) {

  val miniItems =
    CaseMiniMenu(MiniMenu("根目录", None, FileType.folder, true), List("viewer"), List(
      CaseMiniMenu(MiniMenu("喵了个咪", None, FileType.folder, true), List("viewer"), List(
        CaseMiniMenu(MiniMenu("测试页面", Option(controllers.routes.Assets.at("2333.html").url), FileType.leaf, false), List("viewer")),
        CaseMiniMenu(MiniMenu("新建表测试", Option(controllers.routes.DataCreateController.list.url), FileType.leaf, false), List("viewer"))
      ))
    ))

  @tailrec
  final def convert(caseMiniMenu: List[(CaseMiniMenu, Long)], items: List[CaseMenuItem], beginIndex: Long): List[CaseMenuItem] = {
    val childMinis = caseMiniMenu.flatMap { case (caseMini, parentIndex) => caseMini.children.map(_ -> parentIndex) }
    if (! childMinis.isEmpty) {
      val currentInfos = childMinis.zip((beginIndex + 1) to (beginIndex + childMinis.size)).map { case ((caseMini, parentIndex), index) => {
        val menuItem = MenuItem(
          id = Option(index),
          parent = parentIndex,
          name = caseMini.miniMenu.name,
          url = caseMini.miniMenu.url,
          childrenUrl = caseMini.miniMenu.childrenUrl,
          fileType = caseMini.miniMenu.fileType,
          canHaveChild = caseMini.miniMenu.canHaveChild,
          order = index
        )
        CaseMenuItem(menuItem, caseMini.permissions) -> (caseMini -> index)
      } }
      val currentMinis = currentInfos.map(_._2)
      val currentItems = currentInfos.map(_._1)
      val nextBeginIndex = beginIndex + childMinis.size
      convert(currentMinis, items ::: currentItems, nextBeginIndex)
    } else {
      items
    }
  }

  val menuItemsWithPermissions = {
    val firstMenuItem = MenuItem(
      id = Option(1L),
      parent = - 1L,
      name = miniItems.miniMenu.name,
      url = miniItems.miniMenu.url,
      childrenUrl = miniItems.miniMenu.childrenUrl,
      fileType = miniItems.miniMenu.fileType,
      canHaveChild = miniItems.miniMenu.canHaveChild,
      order = 1
    )
    val caseItem = CaseMenuItem(firstMenuItem, miniItems.permissions)
    convert((miniItems, 1L) :: Nil, caseItem :: Nil, 1L)
  }

  val menuItems =
    menuItemsWithPermissions.map(_.menuItem)

}

sealed trait JsonResult {
  val isSuccessed: Boolean
  val message: Option[String]
}
case class JsonData[T](data: Option[T] = None, pageIndex: Option[Int] = None, pageSize: Option[Int] = None, sum: Option[Int] = None, override val message: Option[String] = None, override val isSuccessed: Boolean = true) extends JsonResult