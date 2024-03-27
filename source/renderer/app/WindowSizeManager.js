'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = require('react');
function updateMinScreenHeight(minScreenHeight) {
  const rootWindowEl = document.getElementById('root');
  if (rootWindowEl) {
    rootWindowEl.style.minHeight = minScreenHeight.toString();
  }
}
function WindowSizeManager(props) {
  (0, react_1.useEffect)(() => {
    updateMinScreenHeight(props.minScreenHeight);
  }, [props.minScreenHeight]);
  return null;
}
exports.default = WindowSizeManager;
//# sourceMappingURL=WindowSizeManager.js.map
