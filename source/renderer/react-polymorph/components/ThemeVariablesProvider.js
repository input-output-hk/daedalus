'use strict';
exports.__esModule = true;
exports.ThemeVariablesProvider = void 0;
var react_1 = require('react');
function ThemeVariablesProvider(props) {
  var isRoot = props.isRoot,
    variables = props.variables;
  (0, react_1.useEffect)(function () {
    if (isRoot) {
      // Set css variables on document root
      Object.keys(variables).forEach(function (key) {
        document.documentElement.style.setProperty(key, variables[key]);
      });
    }
  });
  return isRoot ? (
    <>{props.children}</>
  ) : (
    <div style={props.variables}>{props.children}</div>
  );
}
exports.ThemeVariablesProvider = ThemeVariablesProvider;
