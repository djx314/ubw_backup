define('xarcher/tree/Tree', function(require, exports, module) {

	var ko = require('xarcherKo');
	var utils = require('xarcher/common/util/ComponentUtils');
	var treeNodeHtmlText = require('text!xarcher/tree/TreeNode.html');
	var treeItemHtmlText = require('text!xarcher/tree/TreeItem.html');
	var treeHtmlText = require('text!xarcher/tree/Tree.html');

	ko.components.register('archer-tree-node', {
	    viewModel: function(params) {
	        this.data = params.data;
	        this.tree = params.tree;
	    },
	    template: treeNodeHtmlText
	});

	ko.components.register('archer-tree-item', {
	    viewModel: function(params) {
	        this.data = params.data;
	        this.tree = params.tree;
	    },
	    template: treeItemHtmlText
	});

	ko.components.register('archer-tree', {
	    viewModel: function(params) {
	        this.tree = params.tree;
	    },
	    template: treeHtmlText
	});

	var Tree = function(params) {

		var self = this;
	
		self.store = params.store;
		self.id = params.id ? params.id : ComponentUtil.generateId();
		self.componentId = params.componentId ? params.componentId : 'archer_component_' + self.id;
		
	};

    Tree.prototype.itemClick = function(data, event) {};
	
	return Tree;
	
});