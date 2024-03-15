// @flow
import { escapeRegExp } from 'lodash/string';
import React, { Component } from 'react';
import BigNumber from 'bignumber.js';

// $FlowFixMe
import type { SyntheticInputEvent, Element, ElementRef } from 'react';

// external libraries

// internal utility functions
import { withTheme } from './HOC/withTheme';

// import constants
import { removeCharAtPosition } from '../utils/strings';
import type { InputEvent } from '../utils/types';
import { Input } from './Input';
import type { InputProps } from './Input';

type NumericInputValue = null | number | string | BigNumber.Instance;

export type NumericInputProps = InputProps & {
  allowSigns?: boolean,
  allowOnlyIntegers?: boolean,
  bigNumberFormat?: BigNumber.Format,
  decimalPlaces?: number,
  roundingMode?: BigNumber.RoundingMode,
  value: NumericInputValue,
};

type State = {
  composedTheme: Object,
  inputCaretPosition: number,
  fallbackInputValue: ?string,
};

class NumericInputBase extends Component<NumericInputProps, State> {
  inputElement: { current: null | ElementRef<'input'> };
  _hasInputBeenChanged: boolean = false;

  static displayName = 'NumericInput';

  static defaultProps = {
    allowSigns: true,
    allowOnlyIntegers: false,
    readOnly: false,
    roundingMode: BigNumber.ROUND_FLOOR,
    value: null,
  };

  constructor(props: NumericInputProps) {
    super(props);
    this.inputElement = React.createRef();
    this.state = {
      inputCaretPosition: 0,
      fallbackInputValue: null,
    };
  }

  componentDidMount() {
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
  }

  componentDidUpdate(prevProps: NumericInputProps, prevState: State) {
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
  }

  onChange = (newValue, event: SyntheticInputEvent<Element<'input'>>) => {
    const { value, onChange } = this.props;
    const result = this.processValueChange(event.nativeEvent);
    if (result) {
      this._hasInputBeenChanged = true;
      const hasValueChanged = value !== result.value;
      if (hasValueChanged && onChange) {
        onChange(result.value, event);
      }
      this.setState({
        inputCaretPosition: result.caretPosition,
        fallbackInputValue: result.fallbackInputValue,
      });
    }
  };

  /**
   * 1. Handle edge cases that don't need further processing
   * 2. Clean the given value
   * 3. Final processing
   */
  processValueChange(
    event: InputEvent
  ): ?{
    value: NumericInputValue,
    caretPosition: number,
    fallbackInputValue?: ?string,
  } {
    const { allowSigns, allowOnlyIntegers, decimalPlaces, value } = this.props;
    const { inputType, target } = event;
    const { decimalSeparator, groupSeparator } = this.getBigNumberFormat();
    const changedCaretPosition = target.selectionStart;
    const valueToProcess = target.value;
    const fallbackInputValue = this.state.fallbackInputValue;
    const isBackwardDelete = inputType === 'deleteContentBackward';
    const isForwardDelete = inputType === 'deleteContentForward';
    const isDeletion = isForwardDelete || isBackwardDelete;
    const isInsert = inputType === 'insertText';
    const deleteCaretCorrection = isBackwardDelete ? 0 : 1;
    const validInputSignsRegExp = new RegExp(
      `^([-])?([0-9${decimalSeparator}${groupSeparator}]+)?$`
    );
    const validInputNoSignsRegExp = new RegExp(
      `^([0-9${decimalSeparator}${groupSeparator}]+)?$`
    );
    const validInputOnlyIntegersRegExp = new RegExp(`^([0-9]+)?$`);
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
      value == null ? new BigNumber('0') : new BigNumber(value);
    const currentValue =
      fallbackInputValue ?? this.valueToFormattedString(currentNumber);

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
      newValue = removeCharAtPosition(
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
        fallbackInputValue: decimalPlaces > 0 ? null : `0${decimalSeparator}`,
      };
    }

    // Case: Decimal separator was added at the beginning of number
    if (newValue.charAt(0) === decimalSeparator) {
      const newCaretPos = isInsert ? 2 : 1;
      return {
        value: this.bigNumberToFixed(new BigNumber(`0.${newValue.substr(1)}`)),
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
  }

  setInputCaretPosition = (position: number) => {
    const { inputElement } = this;
    if (!inputElement.current) return;
    const input = inputElement.current;
    input.selectionStart = position;
    input.selectionEnd = position;
  };

  focus = () => {
    const { inputElement } = this;
    if (!inputElement.current) return;
    inputElement.current.focus();
  };

  onBlur = (event) => {
    this.setState({
      fallbackInputValue: null,
    });
    this.props.onBlur?.(event);
  };

  getBigNumberFormat(): BigNumber.Config {
    return this.props.bigNumberFormat ?? BigNumber.config().FORMAT;
  }

  valueToFormattedString(number: NumericInputValue) {
    const {
      bigNumberFormat,
      decimalPlaces,
      roundingMode,
      allowOnlyIntegers,
    } = this.props;
    const debugSetting = BigNumber.DEBUG;
    if (BigNumber.isBigNumber(number) && number.isNaN()) return '';
    try {
      BigNumber.DEBUG = true;
      return allowOnlyIntegers
        ? new BigNumber(number).toString()
        : new BigNumber(number).toFormat(decimalPlaces, roundingMode, {
            ...BigNumber.config().FORMAT, // defaults
            ...bigNumberFormat, // custom overrides;
          });
    } catch (e) {
      return '';
    } finally {
      BigNumber.DEBUG = debugSetting;
    }
  }

  bigNumberToFixed(number: BigNumber.Instance) {
    const { decimalPlaces, roundingMode } = this.props;
    return number.toFixed(decimalPlaces, roundingMode);
  }

  formattedValueToBigNumber(value: string) {
    const { decimalSeparator, groupSeparator } = this.getBigNumberFormat();
    return new BigNumber(
      value
        .replace(escapedGlobalRegExp(groupSeparator), '')
        .replace(escapedGlobalRegExp(decimalSeparator), '.')
    );
  }

  getNumberOfGroupSeparators(value: string): number {
    const { groupSeparator } = this.getBigNumberFormat();
    return (value.match(escapedGlobalRegExp(groupSeparator)) || []).length;
  }

  getNumberOfDecimalSeparators(value: string): number {
    const { decimalSeparator } = this.getBigNumberFormat();
    return (value.match(escapedGlobalRegExp(decimalSeparator)) || []).length;
  }

  isSameValue(first: ?BigNumber.Instance, second: ?BigNumber.Instance) {
    return BigNumber.isBigNumber(first)
      ? first.isEqualTo(second)
      : first === second;
  }

  render() {
    // destructuring props ensures only the "...rest" get passed down
    const { onChange, value, ...rest } = this.props;

    const inputValue = this.state.fallbackInputValue
      ? this.state.fallbackInputValue
      : this.valueToFormattedString(value);

    return (
      <Input
        inputRef={this.inputElement}
        onChange={this.onChange}
        onBlur={this.onBlur}
        value={inputValue}
        {...rest}
      />
    );
  }
}

export const NumericInput = withTheme(NumericInputBase);

function escapedGlobalRegExp(regex) {
  return new RegExp(escapeRegExp(regex), 'g');
}
