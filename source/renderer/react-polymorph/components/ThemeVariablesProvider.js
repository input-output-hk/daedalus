exports.__esModule = true;
exports.ThemeVariablesProvider = void 0;
const react_1 = require('react');

function ThemeVariablesProvider(props) {
  const { isRoot } = props;
  const { variables } = props;
  (0, react_1.useEffect)(() => {
    if (isRoot) {
      // Set css variables on document root
      Object.keys(variables).forEach((key) => {
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
