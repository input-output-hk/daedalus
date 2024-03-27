'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
const mobx_react_1 = require('mobx-react');
const context_1 = require('../context');
const DiscreetModeFeatureInject = ({ children }) => {
  const discreetModeFeature = (0, context_1.useDiscreetModeFeature)();
  return children(discreetModeFeature);
};
exports.default = (0, mobx_react_1.observer)(DiscreetModeFeatureInject);
//# sourceMappingURL=DiscreetModeFeatureInject.js.map
