'use strict';
var __createBinding =
  (this && this.__createBinding) ||
  (Object.create
    ? function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        var desc = Object.getOwnPropertyDescriptor(m, k);
        if (
          !desc ||
          ('get' in desc ? !m.__esModule : desc.writable || desc.configurable)
        ) {
          desc = {
            enumerable: true,
            get: function () {
              return m[k];
            },
          };
        }
        Object.defineProperty(o, k2, desc);
      }
    : function (o, m, k, k2) {
        if (k2 === undefined) k2 = k;
        o[k2] = m[k];
      });
var __setModuleDefault =
  (this && this.__setModuleDefault) ||
  (Object.create
    ? function (o, v) {
        Object.defineProperty(o, 'default', { enumerable: true, value: v });
      }
    : function (o, v) {
        o['default'] = v;
      });
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
var __importStar =
  (this && this.__importStar) ||
  function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null)
      for (var k in mod)
        if (k !== 'default' && Object.prototype.hasOwnProperty.call(mod, k))
          __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
  };
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const WalletUtxoCursor_scss_1 = __importDefault(
  require('./WalletUtxoCursor.scss')
);
const OFFSET_TOP = 20;
const OFFSET_BOTTOM = 60;
let WalletUtxoCursor = class WalletUtxoCursor extends react_1.Component {
  render() {
    let { x, width, height } = this.props;
    const { offsetWidth } = this.props;
    // Avoid flow errors for props from HOC
    x = x || 0;
    width = width || 0;
    height = height || 0;
    const verticalPosition = height + OFFSET_TOP + OFFSET_BOTTOM;
    const calculatedWidth = offsetWidth ? (width - offsetWidth) / 2 : 0;
    return react_1.default.createElement(
      'g',
      {
        transform: `translate(${x + calculatedWidth},${-OFFSET_TOP})`,
        className: WalletUtxoCursor_scss_1.default.component,
      },
      react_1.default.createElement('path', {
        d: `M 0.0,0 h ${offsetWidth || 0} v ${verticalPosition} h -${
          offsetWidth || 0
        } Z`,
      })
    );
  }
};
WalletUtxoCursor = __decorate([mobx_react_1.observer], WalletUtxoCursor);
exports.default = WalletUtxoCursor;
//# sourceMappingURL=WalletUtxoCursor.js.map
