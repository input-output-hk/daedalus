exports.__esModule = true;
exports.getDisplayName = exports.hasProperty = exports.numberToPx = exports.composeFunctions = exports.pickDOMProps = void 0;
const filter_invalid_dom_props_1 = require('filter-invalid-dom-props');
// filters out / prevents invalid props from being rendered to the dom
// which would generate an error/warning
exports.pickDOMProps = filter_invalid_dom_props_1.default;
const composeFunctions = function () {
  const fns = [];
  for (let _i = 0; _i < arguments.length; _i++) {
    fns[_i] = arguments[_i];
  }
  return function () {
    const args = [];
    for (let _i = 0; _i < arguments.length; _i++) {
      args[_i] = arguments[_i];
    }
    return fns.forEach((fn) => {
      return fn && fn.apply(void 0, args);
    });
  };
};
exports.composeFunctions = composeFunctions;
const numberToPx = function (val) {
  return typeof val === 'number' ? ''.concat(val, 'px') : val;
};
exports.numberToPx = numberToPx;
const hasProperty = function (obj, property) {
  return Object.prototype.hasOwnProperty.call(obj, property);
};
exports.hasProperty = hasProperty;
const getDisplayName = function (Component) {
  return Component.displayName || Component.name;
};
exports.getDisplayName = getDisplayName;
