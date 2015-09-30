define (require, exports, module) ->

  $ = require "jquery"
  ko = require "xarcherKo"
  TreeStore = require "xarcher/tree/TreeStore"
  Tree = require "xarcher/tree/Tree"

  $ ->

    class TabContent
      constructor: ->
        @currentTab = ko.observable(null)
        @data = ko.mapping.fromJS([])

      closeTab: (data) ->
        @data.remove(data)
        if (@currentTab() is data)
          currentTabData = if @data().length <= 0 then null else @data()[@data().length - 1]
          @currentTab(currentTabData)

    IndexTree = (enumaIndex) ->
      class IndexTree extends Tree
        constructor: (params) ->
          super(params)
        itemClick: (data, event) ->
          super()
          if data[@store.fileTypeProperty]() is "leaf" && data.url()?
            tabData = ko.mapping.fromJS(ko.mapping.toJS(data))
            enumaIndex.tabContent.currentTab(tabData)
            enumaIndex.tabContent.data.push(tabData)

    class EnumaIndex
      constructor: ->
        IndexTreeClass = IndexTree(@)
        @tree = new IndexTreeClass {
          "id": "menuTree"
          "store": new TreeStore {
            "data": []
          }
        }
        @tabContent = new TabContent()

    enumaIndex = new EnumaIndex()

    ko.applyBindings(enumaIndex)

    $.getJSON jsRoutes.controllers.MenuItemController.list().url, (result) =>
      if result.isSuccessed is true
        enumaIndex.tree.store.data(ko.mapping.fromJS(result.data)())