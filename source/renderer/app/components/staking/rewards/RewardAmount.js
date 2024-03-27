'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.RewardAmount = void 0;
const react_1 = __importDefault(require('react'));
const discreet_mode_1 = require('../../../features/discreet-mode');
function RewardAmount({ amount }) {
  // @ts-ignore ts-migrate(2741) FIXME: Property 'replacer' is missing in type '{ children... Remove this comment to see the full error message
  return react_1.default.createElement(
    discreet_mode_1.DiscreetValue,
    null,
    amount
  );
}
exports.RewardAmount = RewardAmount;
//# sourceMappingURL=RewardAmount.js.map
