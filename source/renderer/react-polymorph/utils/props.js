'use strict';
exports.__esModule = true;
exports.getDisplayName = exports.hasProperty = exports.numberToPx = exports.composeFunctions = exports.pickDOMProps = void 0;
var filter_invalid_dom_props_1 = require('filter-invalid-dom-props');
// filters out / prevents invalid props from being rendered to the dom
// which would generate an error/warning
exports.pickDOMProps = filter_invalid_dom_props_1['default'];
var composeFunctions = function () {
  var fns = [];
  for (var _i = 0; _i < arguments.length; _i++) {
    fns[_i] = arguments[_i];
  }
  return function () {
    var args = [];
    for (var _i = 0; _i < arguments.length; _i++) {
      args[_i] = arguments[_i];
    }
    return fns.forEach(function (fn) {
      return fn && fn.apply(void 0, args);
    });
  };
};
exports.composeFunctions = composeFunctions;
var numberToPx = function (val) {
  return typeof val === 'number' ? ''.concat(val, 'px') : val;
};
exports.numberToPx = numberToPx;
var hasProperty = function (obj, property) {
  return Object.prototype.hasOwnProperty.call(obj, property);
};
exports.hasProperty = hasProperty;
var getDisplayName = function (Component) {
  return Component.displayName || Component.name;
};
exports.getDisplayName = getDisplayName;
