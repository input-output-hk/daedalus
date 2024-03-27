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
// @ts-nocheck
const react_1 = __importStar(require('react'));
const mobx_react_1 = require('mobx-react');
const classnames_1 = __importDefault(require('classnames'));
const react_intl_1 = require('react-intl');
const Input_1 = require('@react-polymorph/components/Input');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const global_messages_1 = __importDefault(
  require('../../../i18n/global-messages')
);
const ReadOnlyInput_scss_1 = __importDefault(require('./ReadOnlyInput.scss'));
let ReadOnlyInput = class ReadOnlyInput extends react_1.Component {
  static contextTypes = {
    intl: react_intl_1.intlShape.isRequired,
  };
  render() {
    const { label, value, onClick, isSet, withButton } = this.props;
    const { intl } = this.context;
    const buttonLabel = intl.formatMessage(
      global_messages_1.default[isSet ? 'change' : 'create']
    );
    const mainClasses = (0, classnames_1.default)([
      ReadOnlyInput_scss_1.default.component,
      isSet ? 'changeLabel' : 'createLabel',
    ]);
    return react_1.default.createElement(
      'div',
      { className: mainClasses },
      react_1.default.createElement(Input_1.Input, {
        themeOverrides: ReadOnlyInput_scss_1.default,
        type: 'text',
        label: label,
        value: value,
        disabled: true,
        skin: InputSkin_1.InputSkin,
      }),
      withButton &&
        react_1.default.createElement(
          'button',
          { className: ReadOnlyInput_scss_1.default.button, onClick: onClick },
          buttonLabel
        )
    );
  }
};
ReadOnlyInput = __decorate([mobx_react_1.observer], ReadOnlyInput);
exports.default = ReadOnlyInput;
//# sourceMappingURL=ReadOnlyInput.js.map
