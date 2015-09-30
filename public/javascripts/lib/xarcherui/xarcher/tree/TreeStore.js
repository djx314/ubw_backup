define('xarcher/tree/TreeStore', function(require, exports, module) {

	var ko = require('xarcherKo');
	var $ = require('jquery');

	var TreeStore = function(param) {
		
		var self = this;

		self.data = ko.mapping.fromJS(param.data);
		self.viewData = {};
		
		self.idProperty = param.idProperty ? param.idProperty : 'id';
		self.parentProperty = param.parentProperty ? param.parentProperty : 'parent';
		self.nameProperty = param.nameProperty ? param.nameProperty : 'name';
		self.urlProperty = param.urlProperty ? param.urlProperty : 'url';
		self.childrenUrlProperty = param.childrenUrlProperty ? param.childrenUrlProperty : 'childrenUrl';
		self.fileTypeProperty = param.fileTypeProperty ? param.fileTypeProperty : 'fileType';
		self.canHaveChildProperty = param.canHaveChildProperty ? param.canHaveChildProperty : 'canHaveChild';
		self.orderProperty = param.orderProperty ? param.orderProperty : 'order';

		//这几个不能简单的用for,要遍历整个数组方便触发各个元素change的事件
		//获取一个节点的所有children节点
		self.getChildren = function(parent) {
			var children = $(self.data()).filter(function(index, item) {
				if (item[self.parentProperty]() === parent[self.idProperty]()) {
					return item;
				}
			});
			return children.toArray();
		};
		
		//判断该节点是否root节点
		self.isRoot = function(node) {
			return - 1 === node[self.parentProperty]();
		};
		
		//获取节点深度
		self.nodeDeep = function(node) {
			var deep = 0;
			var currentNode = node;
			while(true) {
                currentNode = self.getParent(currentNode);
                if (currentNode != null) {
                    deep++;
                } else {
                    break;
                }
			}
			return deep;
		};
		
		//刷新所有节点的深度
		self._calculateNodesDeep = function() {
			$(self.data()).each(function(index, item) {
				var viewData = self.viewData[item[self.idProperty]()];
				viewData.deep(self.nodeDeep(item));
			});
		}

		//获取root节点
		self.getRoot = function() {
			var roots = $(self.data()).map(function(index, item) {
				if (self.isRoot(item)) {
					return item;
				}
			});
			return roots[0];
		};

		//获取一个节点的父节点
		self.getParent = function(node) {
			var parent = $(self.data()).map(function(index, item) {
				if (node[self.parentProperty]() === item[self.idProperty]()) {
					return item;
				}
			});
			if (parent.length > 0) {
				return parent[0];
			}
            return null;
		};

		//监控数组变化
		self.data.subscribe(function(changes) {

			$(changes).each(function(index, change) {
				if (change.status === 'added') {
					var viewData = self.viewData[change.value[self.idProperty]()] = ko.mapping.fromJS({
						'isOpened': true,
						'deep': 1
					});
				} else if (change.status === 'deleted') {
					delete self.viewData[change.value[self.idProperty]()];
				}
			});
			
			self._calculateNodesDeep();

		}, null, 'arrayChange');
		
		//关闭所有节点
		self.closeAllTabs = function() {
			$(self.data()).each(function(index, item) {
				self.viewData[item[self.idProperty]()].isOpened(false);
			});
		};
		
		//打开所有节点
		self.openAllTabs = function() {
			$(self.data()).each(function(index, item) {
				self.viewData[item[self.idProperty]()].isOpened(true);
			});
		};

	};
	
	return TreeStore;
	
});