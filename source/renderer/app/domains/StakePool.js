'use strict';
var __decorate =
  (this && this.__decorate) ||
  function (decorators, target, key, desc) {
    var c = arguments.length,
      r =
        c < 3
          ? target
          : desc === null
          ? (desc = Object.getOwnPropertyDescriptor(target, key))
          : desc,
      d;
    if (typeof Reflect === 'object' && typeof Reflect.decorate === 'function')
      r = Reflect.decorate(decorators, target, key, desc);
    else
      for (var i = decorators.length - 1; i >= 0; i--)
        if ((d = decorators[i]))
          r = (c < 3 ? d(r) : c > 3 ? d(target, key, r) : d(target, key)) || r;
    return c > 3 && r && Object.defineProperty(target, key, r), r;
  };
var __metadata =
  (this && this.__metadata) ||
  function (k, v) {
    if (typeof Reflect === 'object' && typeof Reflect.metadata === 'function')
      return Reflect.metadata(k, v);
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.DelegationActions = void 0;
const mobx_1 = require('mobx');
const bignumber_js_1 = __importDefault(require('bignumber.js'));
exports.DelegationActions = {
  CHANGE_DELEGATION: 'changeDelegation',
  REMOVE_DELEGATION: 'removeDelegation',
  DELEGATE: 'delegate',
};
class StakePool {
  id;
  ticker;
  homepage;
  producedBlocks;
  potentialRewards;
  nonMyopicMemberRewards;
  relativeStake;
  pledge;
  cost;
  description = '';
  isCharity;
  name = '';
  profitMargin;
  ranking;
  retiring;
  saturation;
  constructor(data) {
    Object.assign(this, data);
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  StakePool.prototype,
  'ticker',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  StakePool.prototype,
  'homepage',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  StakePool.prototype,
  'producedBlocks',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.default)],
  StakePool.prototype,
  'potentialRewards',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  StakePool.prototype,
  'nonMyopicMemberRewards',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.default)],
  StakePool.prototype,
  'relativeStake',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.default)],
  StakePool.prototype,
  'pledge',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.default)],
  StakePool.prototype,
  'cost',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakePool.prototype,
  'description',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Boolean)],
  StakePool.prototype,
  'isCharity',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  StakePool.prototype,
  'name',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  StakePool.prototype,
  'profitMargin',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  StakePool.prototype,
  'ranking',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Date)],
  StakePool.prototype,
  'retiring',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  StakePool.prototype,
  'saturation',
  void 0
);
exports.default = StakePool;
//# sourceMappingURL=StakePool.js.map
