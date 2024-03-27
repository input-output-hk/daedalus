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
Object.defineProperty(exports, '__esModule', { value: true });
exports.AddressStyles = void 0;
const mobx_1 = require('mobx');
exports.AddressStyles = {
  ADDRESS_BYRON: 'Byron',
  ADDRESS_SHELLEY: 'Shelley',
  ADDRESS_ICARUS: 'Icarus',
};
class WalletAddress {
  id = '';
  used = false;
  spendingPath = "1852'/1815'/0'";
  constructor(data) {
    Object.assign(this, data);
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletAddress.prototype,
  'id',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletAddress.prototype,
  'used',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  WalletAddress.prototype,
  'spendingPath',
  void 0
);
exports.default = WalletAddress;
//# sourceMappingURL=WalletAddress.js.map
