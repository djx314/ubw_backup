define(function (require, exports, module) {

	var ko = require("knockout");
	ko.mapping = require("knockout.mapping");
	//ko.mapping.merge = require("knockout.mapping.merge");
	var $ = require("jquery");
	var utils = require("xarcher/common/util/ComponentUtils");

	ko.bindingHandlers.hoverClass = {

	    init: function (element, valueAccessor) {
	        var value = valueAccessor();
	        var newValue = ko.utils.unwrapObservable(value);
	        /*ko.applyBindingsToNode(element, {
	            event: {
	                mouseenter: function () { $(element).addClass(newValue); },
	                mouseleave: function () { $(element).removeClass(newValue); }
	            }
	        });*/
            $(element).mouseenter(function() {
                $(this).addClass(newValue);
            });
            $(element).mouseleave(function() {
                $(this).removeClass(newValue);
            });
	    }
	    
	};

    ko.bindingHandlers.jqClick = {

        init: function (element, valueAccessor) {
            var func = valueAccessor();
            $(element).click(func);
        }

    };
	
	ko.bindingHandlers.slideVisible = {
			
		update: function(element, valueAccessor, allBindings) {
			
			var childrenNode = $(element);
		
			var value = valueAccessor();
			
			var valueUnwrapped = ko.unwrap(value);
			 
			if (true === valueUnwrapped)
				childrenNode.stop(true, false).show("fast");
			else
				childrenNode.stop(true, false).hide("fast");
			
		}
	    
	};

	ko.bindingHandlers.numericValue = {
		init : function(element, valueAccessor, allBindings, data, context) {
			var precision = null;
			var message = null;
			var unwrapPrecision = parseInt(ko.utils.unwrapObservable(allBindings().precision));
			var unwrapMessage = ko.utils.unwrapObservable(allBindings().message);
			if (unwrapPrecision != null) {
				precision = unwrapPrecision;
			} else {
				precision = ko.bindingHandlers.numericValue.defaultPrecision;
			}
			if (unwrapMessage != null) {
				message = unwrapMessage;
			} else {
				message = ko.bindingHandlers.numericValue.defaultMessage;
			}
			var interceptor = ko.computed({
				read: function() {
					var value = ko.unwrap(valueAccessor());
					var newValue = null;
					var newStringValue = null;
					if (precision >= 0) {
						newStringValue = utils.toDecimal(value, precision);
						newValue = parseFloat(newStringValue);
					} else {
						newValue = parseFloat(value);
						newStringValue = newValue.toString();
					}
					if (!isFinite(newValue)) {
						return message; //留空
					} else {
						return newStringValue;
					}
				},
				write: function(value) {
					var newStrValue = null;
					if (precision >= 0) {
						newStrValue = parseFloat(utils.toDecimal(value, precision));
					} else {
						newStrValue = parseFloat(value);
					}
					if (isFinite(newStrValue)) {
						valueAccessor()(newStrValue);
						$(element).val(utils.toDecimal(newStrValue, precision));
					} else {
						$(element).val(message); //留空
						//valueAccessor()(newStrValue);
						//TODO 这样真的好么
						valueAccessor()(null);
					}
				},
				disposeWhenNodeIsRemoved: element
			});

			ko.applyBindingsToNode(element, { value: interceptor }, context);
		},
		defaultPrecision: 2,
		defaultMessage: "留空"
	};

	ko.bindingHandlers.numericText = {
		init : function(element, valueAccessor, allBindings, data, context) {
			var precision = null;
			var message = null;
			var unwrapPrecision = parseInt(ko.utils.unwrapObservable(allBindings().precision));
			var unwrapMessage = ko.utils.unwrapObservable(allBindings().message);
			if (unwrapPrecision != null) {
				precision = unwrapPrecision;
			} else {
				precision = ko.bindingHandlers.numericValue.defaultPrecision;
			}
			if (unwrapMessage != null) {
				message = unwrapMessage;
			} else {
				message = ko.bindingHandlers.numericValue.defaultMessage;
			}
			var interceptor = ko.computed({
				read: function() {
					var value = ko.unwrap(valueAccessor());
					var newValue = null;
					var newStringValue = null;
					if (precision >= 0) {
						newStringValue = utils.toDecimal(value, precision);
						newValue = parseFloat(newStringValue);
					} else {
						newValue = parseFloat(value);
						newStringValue = newValue.toString();
					}
					if (!isFinite(newValue)) {
						return message; //留空
					} else {
						return newStringValue;
					}
				},
				disposeWhenNodeIsRemoved: element
			});

			ko.applyBindingsToNode(element, { text: interceptor }, context);
		},
		defaultPrecision: 2,
		defaultMessage: "留空"
	};

	return ko;
	
});