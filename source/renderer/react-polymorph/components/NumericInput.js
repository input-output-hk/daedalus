const __extends =
  (this && this.__extends) ||
  (function () {
    var extendStatics = function (d, b) {
      extendStatics =
        Object.setPrototypeOf ||
        ({ __proto__: [] } instanceof Array &&
          function (d, b) {
            d.__proto__ = b;
          }) ||
        function (d, b) {
          for (const p in b)
            if (Object.prototype.hasOwnProperty.call(b, p)) d[p] = b[p];
        };
      return extendStatics(d, b);
    };
    return function (d, b) {
      if (typeof b !== 'function' && b !== null)
        throw new TypeError(
          `Class extends value ${String(b)} is not a constructor or null`
        );
      extendStatics(d, b);
      function __() {
        this.constructor = d;
      }
      d.prototype =
        b === null
          ? Object.create(b)
          : ((__.prototype = b.prototype), new __());
    };
  })();
var __assign =
  (this && this.__assign) ||
  function () {
    __assign =
      Object.assign ||
      function (t) {
        for (var s, i = 1, n = arguments.length; i < n; i++) {
          s = arguments[i];
          for (const p in s)
            if (Object.prototype.hasOwnProperty.call(s, p)) t[p] = s[p];
        }
        return t;
      };
    return __assign.apply(this, arguments);
  };
const __rest =
  (this && this.__rest) ||
  function (s, e) {
    const t = {};
    for (var p in s)
      if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === 'function')
      for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
        if (
          e.indexOf(p[i]) < 0 &&
          Object.prototype.propertyIsEnumerable.call(s, p[i])
        )
          t[p[i]] = s[p[i]];
      }
    return t;
  };
exports.__esModule = true;
exports.NumericInput = void 0;
// @ts-nocheck
const string_1 = require('lodash/string');
const react_1 = require('react');
const bignumber_js_1 = require('bignumber.js');
// external libraries
// internal utility functions
const withTheme_1 = require('./HOC/withTheme');
// import constants
const strings_1 = require('../utils/strings');
const Input_1 = require('./Input');

const NumericInputBase = /** @class */ (function (_super) {
  __extends(NumericInputBase, _super);
  function NumericInputBase(props) {
    const _this = _super.call(this, props) || this;
    _this._hasInputBeenChanged = false;
    _this.onChange = function (newValue, event) {
      const _a = _this.props;
      const { value } = _a;
      const { onChange } = _a;
      const result = _this.processValueChange(event.nativeEvent);
      if (result) {
        _this._hasInputBeenChanged = true;
        const hasValueChanged = value !== result.value;
        if (hasValueChanged && onChange) {
          onChange(result.value, event);
        }
        _this.setState({
          inputCaretPosition: result.caretPosition,
          fallbackInputValue: result.fallbackInputValue,
        });
      }
    };
    _this.setInputCaretPosition = function (position) {
      const { inputElement } = _this;
      if (!inputElement.current) return;
      const input = inputElement.current;
      input.selectionStart = position;
      input.selectionEnd = position;
    };
    _this.focus = function () {
      const { inputElement } = _this;
      if (!inputElement.current) return;
      inputElement.current.focus();
    };
    _this.onBlur = function (event) {
      let _a;
      let _b;
      _this.setState({
        fallbackInputValue: null,
      });
      (_b = (_a = _this.props).onBlur) === null || _b === void 0
        ? void 0
        : _b.call(_a, event);
    };
    _this.inputElement = react_1.default.createRef();
    _this.state = {
      inputCaretPosition: 0,
      fallbackInputValue: null,
    };
    return _this;
  }
  NumericInputBase.prototype.componentDidMount = function () {
    const { inputElement } = this;
    const { autoFocus } = this.props;
    if (autoFocus) {
      this.focus();
      if (inputElement && inputElement.current) {
        this.setState({
          inputCaretPosition: inputElement.current.selectionStart,
        });
      }
    }
  };
  NumericInputBase.prototype.componentDidUpdate = function (
    prevProps,
    prevState
  ) {
    const { value } = this.props;
    const { inputCaretPosition } = this.state;
    const hasValueBeenChanged = value !== prevProps.value;
    const hasCaretBeenChanged =
      inputCaretPosition !== prevState.inputCaretPosition;
    if (
      this._hasInputBeenChanged ||
      hasValueBeenChanged ||
      hasCaretBeenChanged
    ) {
      this.setInputCaretPosition(inputCaretPosition);
    }
    this._hasInputBeenChanged = false;
  };
  /**
   * 1. Handle edge cases that don't need further processing
   * 2. Clean the given value
   * 3. Final processing
   */
  NumericInputBase.prototype.processValueChange = function (event) {
    const _a = this.props;
    const { allowSigns } = _a;
    const { allowOnlyIntegers } = _a;
    const { decimalPlaces } = _a;
    const { value } = _a;
    const { inputType } = event;
    const { target } = event;
    const _b = this.getBigNumberFormat();
    const { decimalSeparator } = _b;
    const { groupSeparator } = _b;
    const changedCaretPosition = target.selectionStart;
    const valueToProcess = target.value;
    const { fallbackInputValue } = this.state;
    const isBackwardDelete = inputType === 'deleteContentBackward';
    const isForwardDelete = inputType === 'deleteContentForward';
    const isDeletion = isForwardDelete || isBackwardDelete;
    const isInsert = inputType === 'insertText';
    const deleteCaretCorrection = isBackwardDelete ? 0 : 1;
    const validInputSignsRegExp = new RegExp(
      '^([-])?([0-9'.concat(decimalSeparator).concat(groupSeparator, ']+)?$')
    );
    const validInputNoSignsRegExp = new RegExp(
      '^([0-9'.concat(decimalSeparator).concat(groupSeparator, ']+)?$')
    );
    const validInputOnlyIntegersRegExp = new RegExp('^([0-9]+)?$');
    let validInputRegex = allowSigns
      ? validInputSignsRegExp
      : validInputNoSignsRegExp;
    validInputRegex = allowOnlyIntegers
      ? validInputOnlyIntegersRegExp
      : validInputRegex;
    const valueHasLeadingZero = /^0[1-9]/.test(valueToProcess);
    /**
     * ========= HANDLE HARD EDGE-CASES =============
     */
    // Case: invalid characters entered -> refuse!
    if (!validInputRegex.test(valueToProcess)) {
      return {
        caretPosition: changedCaretPosition - 1,
        fallbackInputValue,
        value,
      };
    }
    // Case: Everything was deleted -> reset state
    if (valueToProcess === '') {
      return {
        value: null,
        caretPosition: 0,
        fallbackInputValue: null,
      };
    }
    // Case: value is the same as the fallback (which is always shown if defined)
    if (valueToProcess === this.state.fallbackInputValue) return null;
    // Case: Just minus sign was entered
    if (valueToProcess === '-') {
      return {
        value: null,
        caretPosition: 1,
        fallbackInputValue: '-',
      };
    }
    // Case: Just minus sign was entered
    if (valueToProcess === groupSeparator) {
      return {
        value: null,
        caretPosition: 0,
        fallbackInputValue: null,
      };
    }
    /**
     * ========= CLEAN THE INPUT =============
     */
    const currentNumber =
      value == null
        ? new bignumber_js_1.default('0')
        : new bignumber_js_1.default(value);
    const currentValue =
      fallbackInputValue !== null && fallbackInputValue !== void 0
        ? fallbackInputValue
        : this.valueToFormattedString(currentNumber);
    const currentNumberOfDecimalSeparators = this.getNumberOfDecimalSeparators(
      currentValue
    );
    const hadDecimalSeparatorBefore = currentNumberOfDecimalSeparators > 0;
    // New Value
    let newValue = valueToProcess;
    let newCaretPosition = changedCaretPosition;
    const newNumberOfDecimalSeparators = this.getNumberOfDecimalSeparators(
      newValue
    );
    // Case: A second decimal separator was added somewhere
    if (hadDecimalSeparatorBefore && newNumberOfDecimalSeparators === 2) {
      const oldFirstIndex = currentValue.indexOf(decimalSeparator);
      const newFirstIndex = newValue.indexOf(decimalSeparator);
      const wasSeparatorAddedBeforeOldOne = newFirstIndex < oldFirstIndex;
      // Remove the second decimal point and set caret position
      newValue = (0, strings_1.removeCharAtPosition)(
        newValue,
        wasSeparatorAddedBeforeOldOne
          ? newValue.lastIndexOf(decimalSeparator)
          : oldFirstIndex
      );
      newCaretPosition = newValue.indexOf(decimalSeparator) + 1;
    }
    // Case: Decimal separator was replaced with a number
    const newValueHasTrailingZeros = new RegExp('^[1-9]+0+$');
    if (
      !!decimalPlaces &&
      value != null &&
      hadDecimalSeparatorBefore &&
      newNumberOfDecimalSeparators === 0 &&
      isInsert &&
      newValue.length > 1 &&
      !newValueHasTrailingZeros.test(newValue)
    ) {
      return {
        caretPosition: changedCaretPosition - 1,
        fallbackInputValue,
        value,
      };
    }
    /**
     * ========= PROCESS CLEANED INPUT =============
     */
    // Case: Just a decimal separator was entered
    if (newValue === decimalSeparator) {
      return {
        value: '0',
        caretPosition: 2,
        fallbackInputValue:
          decimalPlaces > 0 ? null : '0'.concat(decimalSeparator),
      };
    }
    // Case: Decimal separator was added at the beginning of number
    if (newValue.charAt(0) === decimalSeparator) {
      const newCaretPos = isInsert ? 2 : 1;
      return {
        value: this.bigNumberToFixed(
          new bignumber_js_1.default('0.'.concat(newValue.substr(1)))
        ),
        caretPosition: newCaretPos,
        fallbackInputValue: null,
      };
    }
    const newNumber =
      newValue === '' ? null : this.formattedValueToBigNumber(newValue);
    // Case: Invalid change has been made -> ignore it
    if (newNumber == null) {
      const deleteAdjustment = isBackwardDelete ? 0 : 1; // special cases when deleting dot
      const insertAdjustment = -1; // don't move caret if numbers are "inserted"
      return {
        caretPosition:
          changedCaretPosition +
          (isDeletion ? deleteAdjustment : insertAdjustment),
        fallbackInputValue,
        value: this.bigNumberToFixed(currentNumber),
      };
    }
    const formattedNewNumber = this.valueToFormattedString(newNumber);
    // Case: Dot was added at the end of number
    if (
      !isDeletion &&
      newValue.charAt(newValue.length - 1) === decimalSeparator
    ) {
      return {
        value: this.bigNumberToFixed(newNumber),
        caretPosition: changedCaretPosition,
        fallbackInputValue:
          decimalPlaces > 0 ? null : formattedNewNumber + decimalSeparator,
        minimumFractionDigits: 0,
      };
    }
    // Case: Decimal separator was deleted while number of decimal places specified
    const hasDecimalPlaces = decimalPlaces != null;
    const wasDecimalSeparatorRemoved =
      hadDecimalSeparatorBefore && !newNumberOfDecimalSeparators;
    const newValueSlicedAtNewInputtedNumber = newValue.slice(
      1,
      newValue.length
    );
    const newTrailingNumbersAreAllZero = /^0+$/.test(
      newValueSlicedAtNewInputtedNumber
    );
    if (wasDecimalSeparatorRemoved && hasDecimalPlaces && !isInsert) {
      return {
        caretPosition: newCaretPosition + deleteCaretCorrection,
        fallbackInputValue: null,
        value: this.bigNumberToFixed(currentNumber),
      };
    }
    // Edge case for inserts with trailing zeros
    if (
      wasDecimalSeparatorRemoved &&
      hasDecimalPlaces &&
      isInsert &&
      !newTrailingNumbersAreAllZero &&
      newValue.length > 1
    ) {
      return {
        caretPosition: newCaretPosition + deleteCaretCorrection,
        fallbackInputValue: null,
        value: this.bigNumberToFixed(currentNumber),
      };
    }
    // Case: Valid change has been made
    const hasNumberChanged = !this.isSameValue(currentNumber, newNumber);
    const groupSeparatorsDiff =
      this.getNumberOfGroupSeparators(formattedNewNumber) -
      this.getNumberOfGroupSeparators(newValue);
    const hasNumberOfGroupSeparatorsChanged = groupSeparatorsDiff > 0;
    const onlyNumberOfGroupSeparatorsChanged =
      !hasNumberChanged && hasNumberOfGroupSeparatorsChanged;
    const leadingZeroCorrection = valueHasLeadingZero ? -1 : 0;
    const caretCorrection =
      (onlyNumberOfGroupSeparatorsChanged
        ? deleteCaretCorrection
        : groupSeparatorsDiff) + leadingZeroCorrection;
    return {
      caretPosition: Math.max(newCaretPosition + caretCorrection, 0),
      fallbackInputValue: null,
      value: this.bigNumberToFixed(newNumber),
    };
  };
  NumericInputBase.prototype.getBigNumberFormat = function () {
    let _a;
    return (_a = this.props.bigNumberFormat) !== null && _a !== void 0
      ? _a
      : bignumber_js_1.default.config().FORMAT;
  };
  NumericInputBase.prototype.valueToFormattedString = function (number) {
    const _a = this.props;
    const { bigNumberFormat } = _a;
    const { decimalPlaces } = _a;
    const { roundingMode } = _a;
    const { allowOnlyIntegers } = _a;
    const debugSetting = bignumber_js_1.default.DEBUG;
    if (bignumber_js_1.default.isBigNumber(number) && number.isNaN()) return '';
    try {
      bignumber_js_1.default.DEBUG = true;
      return allowOnlyIntegers
        ? new bignumber_js_1.default(number).toString()
        : new bignumber_js_1.default(number).toFormat(
            decimalPlaces,
            roundingMode,
            __assign(
              __assign({}, bignumber_js_1.default.config().FORMAT),
              bigNumberFormat
            )
          );
    } catch (e) {
      return '';
    } finally {
      bignumber_js_1.default.DEBUG = debugSetting;
    }
  };
  NumericInputBase.prototype.bigNumberToFixed = function (number) {
    const _a = this.props;
    const { decimalPlaces } = _a;
    const { roundingMode } = _a;
    return number.toFixed(decimalPlaces, roundingMode);
  };
  NumericInputBase.prototype.formattedValueToBigNumber = function (value) {
    const _a = this.getBigNumberFormat();
    const { decimalSeparator } = _a;
    const { groupSeparator } = _a;
    return new bignumber_js_1.default(
      value
        .replace(escapedGlobalRegExp(groupSeparator), '')
        .replace(escapedGlobalRegExp(decimalSeparator), '.')
    );
  };
  NumericInputBase.prototype.getNumberOfGroupSeparators = function (value) {
    const { groupSeparator } = this.getBigNumberFormat();
    return (value.match(escapedGlobalRegExp(groupSeparator)) || []).length;
  };
  NumericInputBase.prototype.getNumberOfDecimalSeparators = function (value) {
    const { decimalSeparator } = this.getBigNumberFormat();
    return (value.match(escapedGlobalRegExp(decimalSeparator)) || []).length;
  };
  NumericInputBase.prototype.isSameValue = function (first, second) {
    return bignumber_js_1.default.isBigNumber(first)
      ? first.isEqualTo(second)
      : first === second;
  };
  NumericInputBase.prototype.render = function () {
    // destructuring props ensures only the "...rest" get passed down
    const _a = this.props;
    const { onChange } = _a;
    const { value } = _a;
    const rest = __rest(_a, ['onChange', 'value']);
    const inputValue = this.state.fallbackInputValue
      ? this.state.fallbackInputValue
      : this.valueToFormattedString(value);
    return (
      <Input_1.Input
        inputRef={this.inputElement}
        onChange={this.onChange}
        onBlur={this.onBlur}
        value={inputValue}
        {...rest}
      />
    );
  };
  NumericInputBase.displayName = 'NumericInput';
  NumericInputBase.defaultProps = {
    allowSigns: true,
    allowOnlyIntegers: false,
    readOnly: false,
    roundingMode: bignumber_js_1.default.ROUND_FLOOR,
    value: null,
  };
  return NumericInputBase;
})(react_1.Component);
exports.NumericInput = (0, withTheme_1.withTheme)(NumericInputBase);
function escapedGlobalRegExp(regex) {
  return new RegExp((0, string_1.escapeRegExp)(regex), 'g');
}
