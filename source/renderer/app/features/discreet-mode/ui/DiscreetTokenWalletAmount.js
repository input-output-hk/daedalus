'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importDefault(require('react'));
const mobx_react_1 = require('mobx-react');
const discreetWalletTokenAmount_1 = require('../replacers/discreetWalletTokenAmount');
const DiscreetValue_1 = __importDefault(require('./DiscreetValue'));
function DiscreetTokenWalletAmount(props) {
  // @ts-ignore ts-migrate(2741) FIXME: Property 'children' is missing in type '{ replacer... Remove this comment to see the full error message
  return react_1.default.createElement(DiscreetValue_1.default, {
    replacer: (0, discreetWalletTokenAmount_1.discreetWalletTokenAmount)(props),
  });
}
exports.default = (0, mobx_react_1.observer)(DiscreetTokenWalletAmount);
//# sourceMappingURL=DiscreetTokenWalletAmount.js.map
