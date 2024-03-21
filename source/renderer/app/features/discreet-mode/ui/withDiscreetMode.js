'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.withDiscreetMode = void 0;
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const context_1 = require('../context');
function getDisplayName(WrappedComponent) {
  return WrappedComponent.displayName || WrappedComponent.name || 'Component';
}
function withDiscreetMode(WrappedComponent) {
  function WithDiscreetMode(props) {
    const feature = (0, context_1.useDiscreetModeFeature)();
    return react_1.default.createElement(WrappedComponent, {
      ...props,
      discreetModeFeature: feature,
    });
  }
  WithDiscreetMode.displayName = `WithDiscreetMode(${getDisplayName(
    WrappedComponent
  )})`;
  return (0, mobx_react_1.observer)(WithDiscreetMode);
}
exports.withDiscreetMode = withDiscreetMode;
//# sourceMappingURL=withDiscreetMode.js.map
