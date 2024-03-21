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
const classnames_1 = __importDefault(require('classnames'));
const AddressActions_1 = __importDefault(require('./AddressActions'));
const AddressSequential_scss_1 = __importDefault(
  require('./AddressSequential.scss')
);
let AddressSequential = class AddressSequential extends react_1.Component {
  addressElement;
  addressContainerElement;
  componentDidMount() {
    if (this.props.shouldRegisterAddressElement) {
      this.props.onRegisterHTMLElements(
        this.addressElement,
        this.addressContainerElement
      );
    }
  }
  get rawAddress() {
    return this.props.address.id;
  }
  get renderAddress() {
    const { rawAddress } = this;
    const { addressSlice } = this.props;
    if (!addressSlice) return rawAddress;
    const addressBegin = rawAddress.slice(0, addressSlice);
    const addressEnd = rawAddress.slice(-addressSlice);
    return `${addressBegin}…${addressEnd}`;
  }
  render() {
    const {
      address,
      onShareAddress,
      onCopyAddress,
      shouldRegisterAddressElement,
    } = this.props;
    const { renderAddress, rawAddress } = this;
    const addressClasses = (0, classnames_1.default)([
      'Address',
      `receiveAddress-${rawAddress}`,
      AddressSequential_scss_1.default.component,
      address.used ? AddressSequential_scss_1.default.usedWalletAddress : null,
    ]);
    return react_1.default.createElement(
      'div',
      {
        className: addressClasses,
        onClick: () => onShareAddress(address),
        role: 'link',
        'aria-hidden': true,
      },
      react_1.default.createElement(
        'div',
        {
          className: AddressSequential_scss_1.default.addressId,
          ref: (ref) => {
            this.addressContainerElement = ref;
          },
          id: `address-${rawAddress}`,
        },
        renderAddress,
        shouldRegisterAddressElement &&
          react_1.default.createElement(
            'span',
            {
              ref: (ref) => {
                this.addressElement = ref;
              },
              className: AddressSequential_scss_1.default.addressElement,
            },
            rawAddress
          )
      ),
      react_1.default.createElement(AddressActions_1.default, {
        address: address,
        onShareAddress: onShareAddress,
        onCopyAddress: onCopyAddress,
        type: 'share',
      })
    );
  }
};
AddressSequential = __decorate([mobx_react_1.observer], AddressSequential);
exports.default = AddressSequential;
//# sourceMappingURL=AddressSequential.js.map
