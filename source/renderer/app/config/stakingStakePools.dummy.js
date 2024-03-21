'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const stakingStakePools_dummy_json_1 = __importDefault(
  require('./stakingStakePools.dummy.json')
);
exports.default = stakingStakePools_dummy_json_1.default.map(
  ({
    relativeStake,
    potentialRewards,
    cost,
    pledge,
    retiring,
    ...stakePool
  }) => ({
    ...stakePool,
    relativeStake: new bignumber_js_1.default(relativeStake),
    potentialRewards: new bignumber_js_1.default(potentialRewards),
    cost: new bignumber_js_1.default(cost),
    pledge: new bignumber_js_1.default(pledge),
    retiring: new Date(retiring),
  })
);
//# sourceMappingURL=stakingStakePools.dummy.js.map
