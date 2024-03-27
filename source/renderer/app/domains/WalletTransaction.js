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
exports.WalletTransaction = exports.TransactionWithdrawal = exports.TransactionTypes = exports.TransactionStates = void 0;
const mobx_1 = require('mobx');
const bignumber_js_1 = __importDefault(require('bignumber.js'));
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
exports.TransactionStates = {
  PENDING: 'pending',
  OK: 'in_ledger',
  FAILED: 'expired',
};
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
exports.TransactionTypes = {
  CARD: 'card',
  EXPEND: 'expend',
  INCOME: 'income',
  EXCHANGE: 'exchange',
};
exports.TransactionWithdrawal = 'self';
class WalletTransaction {
  id = '';
  type;
  title = '';
  amount;
  fee;
  deposit;
  assets;
  date;
  description = '';
  addresses = {
    from: [],
    to: [],
    withdrawals: [],
  };
  state;
  confirmations;
  slotNumber;
  epochNumber;
  metadata;
  constructor(data) {
    Object.assign(this, data);
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletTransaction.prototype,
  'id',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  WalletTransaction.prototype,
  'type',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletTransaction.prototype,
  'title',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.default)],
  WalletTransaction.prototype,
  'amount',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.default)],
  WalletTransaction.prototype,
  'fee',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', bignumber_js_1.default)],
  WalletTransaction.prototype,
  'deposit',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Array)],
  WalletTransaction.prototype,
  'assets',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Date)],
  WalletTransaction.prototype,
  'date',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletTransaction.prototype,
  'description',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletTransaction.prototype,
  'addresses',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', String)],
  WalletTransaction.prototype,
  'state',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  WalletTransaction.prototype,
  'confirmations',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  WalletTransaction.prototype,
  'slotNumber',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  WalletTransaction.prototype,
  'epochNumber',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletTransaction.prototype,
  'metadata',
  void 0
);
exports.WalletTransaction = WalletTransaction;
//# sourceMappingURL=WalletTransaction.js.map
