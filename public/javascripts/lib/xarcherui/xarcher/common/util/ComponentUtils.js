define('xarcher/common/util/ComponentUtils', function(require, exports, module) {
	
	var currentId = 0;

	var ComponentUtils = function() {
		
		var self = this;
		
		self.generateId = function() {
			return (currentId++).toString();
		};

		self.toDecimal = function(x, precision) {
			var deciNum = 1;
			for (var i = 0; i < precision; i++) {
				deciNum *= 10;
			}

			var f = parseFloat(x);
			if (!isFinite(f)) {
				return NaN;
			}
			var f = Math.round(x * deciNum) / deciNum;
			var s = f.toString();
			if (precision > 0) {
                var rs = s.indexOf('.');
                if (rs < 0) {
                    rs = s.length;
                    s += '.';
                }
                while (s.length <= rs + precision) {
                    s += '0';
                }
            }
			return s;
		}
		
	};
	
	return new ComponentUtils();
	
});