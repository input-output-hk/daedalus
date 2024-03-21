'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.withAnalytics = void 0;
const react_1 = __importDefault(require('react'));
const useAnalytics_1 = require('./useAnalytics');
function withAnalytics(WrappedComponent) {
  const displayName =
    WrappedComponent.displayName || WrappedComponent.name || 'Component';
  function ComponentWithTheme(props) {
    const analyticsTracker = (0, useAnalytics_1.useAnalytics)();
    // props comes afterwards so the can override the default ones.
    return react_1.default.createElement(WrappedComponent, {
      ...props,
      analyticsTracker: analyticsTracker,
    });
  }
  ComponentWithTheme.displayName = `withAnalytics(${displayName})`;
  return ComponentWithTheme;
}
exports.withAnalytics = withAnalytics;
//# sourceMappingURL=withAnalytics.js.map
