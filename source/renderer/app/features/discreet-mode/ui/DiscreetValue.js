'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const context_1 = require('../context');
const config_1 = require('../config');
const defaultReplacer_1 = require('../replacers/defaultReplacer');
function DiscreetValue({
  children,
  replacer = (0, defaultReplacer_1.defaultReplacer)(),
}) {
  const feature = (0, context_1.useDiscreetModeFeature)();
  return react_1.default.createElement(
    react_1.default.Fragment,
    null,
    replacer(feature.isDiscreetMode, config_1.SENSITIVE_DATA_SYMBOL, children)
  );
}
exports.default = (0, mobx_react_1.observer)(DiscreetValue);
//# sourceMappingURL=DiscreetValue.js.map
