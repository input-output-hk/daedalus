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
const lodash_1 = require('lodash');
const NumericInput_1 = require('@react-polymorph/components/NumericInput');
const InputSkin_1 = require('@react-polymorph/skins/simple/InputSkin');
const API_1 = require('@react-polymorph/themes/API');
const PopOver_1 = require('@react-polymorph/components/PopOver');
const classnames_1 = __importDefault(require('classnames'));
const PinCode_scss_1 = __importDefault(require('./PinCode.scss'));
class PinCode extends react_1.Component {
  static defaultProps = {
    length: 4,
    disabled: false,
    value: [],
  };
  inputsRef = [];
  focusKey = 0;
  add = false;
  onChange = (inputValue, key) => {
    const { value, onChange } = this.props;
    const inputNewValue =
      inputValue !== null && inputValue !== undefined
        ? inputValue.toString()
        : '';
    if (
      !Object.prototype.hasOwnProperty.call(value, key) ||
      value[key] === '' ||
      inputNewValue === ''
    ) {
      const newValue = value;
      newValue[key] = inputNewValue;
      if (onChange) {
        onChange(newValue);
      }
      this.focusKey = key;
      this.add = inputValue !== null && inputValue !== undefined;
    }
  };
  componentDidUpdate() {
    const { value, length } = this.props;
    const key = value.join('').length;
    if (key > 0 && key < length) {
      const inputFocusKey = this.add ? this.focusKey + 1 : this.focusKey - 1;
      if (
        Object.prototype.hasOwnProperty.call(this.inputsRef, inputFocusKey) &&
        this.inputsRef[inputFocusKey]
      )
        this.inputsRef[inputFocusKey].focus();
    }
  }
  generatePinCodeInput = () => {
    const {
      id,
      name,
      type,
      autoFocus,
      length,
      error,
      value,
      disabled,
    } = this.props;
    const pinCodeClasses = (0, classnames_1.default)([
      PinCode_scss_1.default.pinCode,
      error ? PinCode_scss_1.default.error : null,
    ]);
    return react_1.default.createElement(
      'div',
      { className: PinCode_scss_1.default.pinCodeInput },
      (0, lodash_1.map)(Array(length).fill(), (action, key) => {
        return react_1.default.createElement(NumericInput_1.NumericInput, {
          ref: (input) => {
            if (
              !Object.prototype.hasOwnProperty.call(this.inputsRef, key) ||
              this.inputsRef[key] !== input
            )
              this.inputsRef[key] = input;
          },
          id: id,
          name: name,
          type: type,
          className: pinCodeClasses,
          label: null,
          key: key,
          themeId: API_1.IDENTIFIERS.INPUT,
          skin: InputSkin_1.InputSkin,
          onChange: (number) => this.onChange(number, key),
          value: value ? value[key] : undefined,
          autoFocus: autoFocus && key === 0,
          disabled:
            disabled ||
            (key !== 0 &&
              (!value ||
                !Object.prototype.hasOwnProperty.call(value, key - 1))),
        });
      })
    );
  };
  render() {
    const { label, error } = this.props;
    const pinCode = this.generatePinCodeInput();
    return react_1.default.createElement(
      'div',
      { className: PinCode_scss_1.default.component, role: 'button' },
      react_1.default.createElement(
        'label',
        { htmlFor: 'firstName', className: 'SimpleFormField_label' },
        label
      ),
      error
        ? react_1.default.createElement(
            PopOver_1.PopOver,
            {
              content: error,
              placement: 'bottom',
              themeVariables: {
                '--rp-pop-over-bg-color': 'var(--theme-color-error)',
              },
            },
            pinCode
          )
        : react_1.default.createElement(react_1.default.Fragment, null, pinCode)
    );
  }
}
exports.default = PinCode;
//# sourceMappingURL=PinCode.js.map
