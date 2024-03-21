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
const lodash_1 = require('lodash');
const mobx_1 = require('mobx');
const strings_1 = require('../utils/strings');
class Asset {
  policyId = '';
  assetName = '';
  uniqueId = '';
  fingerprint = '';
  metadata;
  decimals;
  recommendedDecimals;
  get assetNameASCII() {
    return (0, strings_1.hexToString)(this.assetName || '');
  }
  constructor(props) {
    const { uniqueId } = props;
    Object.assign(this, props, {
      uniqueId,
    });
  }
  update(props) {
    const { uniqueId } = props;
    Object.assign(
      this,
      (0, lodash_1.pick)(props, [
        'policyId',
        'assetName',
        'fingerprint',
        'metadata',
        'decimals',
        'recommendedDecimals',
      ]),
      {
        uniqueId,
      }
    );
  }
}
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Asset.prototype,
  'policyId',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Asset.prototype,
  'assetName',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Asset.prototype,
  'uniqueId',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Asset.prototype,
  'fingerprint',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Object)],
  Asset.prototype,
  'metadata',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  Asset.prototype,
  'decimals',
  void 0
);
__decorate(
  [mobx_1.observable, __metadata('design:type', Number)],
  Asset.prototype,
  'recommendedDecimals',
  void 0
);
__decorate(
  [
    mobx_1.computed,
    __metadata('design:type', Object),
    __metadata('design:paramtypes', []),
  ],
  Asset.prototype,
  'assetNameASCII',
  null
);
__decorate(
  [
    mobx_1.action,
    __metadata('design:type', Function),
    __metadata('design:paramtypes', [Object]),
    __metadata('design:returntype', void 0),
  ],
  Asset.prototype,
  'update',
  null
);
exports.default = Asset;
//# sourceMappingURL=Asset.js.map
