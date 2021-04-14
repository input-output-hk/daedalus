// @flow
import React, { Component } from 'react';
import { map, isNaN } from 'lodash';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classNames from 'classnames';
import BigNumber from 'bignumber.js';
import type { Field } from 'mobx-react-form';
import styles from './PinCode.scss';

type Props = $Exact<{
  id: string,
  name: string,
  type: string,
  autoFocus: boolean,
  onChange?: Function,
  label: string,
  length: number,
  disabled: boolean,
  value: Array<string>,
  error: string | null,
  selectedPinField: ?string,
}>;

export default class PinCode extends Component<Props> {
  static defaultProps = {
    length: 4,
    disabled: false,
    value: [],
  };

  inputsRef = [];
  focusKey = 0;
  add = false;

  inputHasNewValue = (inputNewValue: string, key: number) => {
    const { value } = this.props;
    const valueIsEmpty =
      !value[key] || value[key] === '' || inputNewValue === '';
    const valueHasChanged =
      inputNewValue &&
      inputNewValue.length === 1 &&
      value[key] !== inputNewValue;
    return valueIsEmpty || valueHasChanged;
  };

  onChange = (inputValue: ?number | ?string, key: number) => {
    const { value, onChange } = this.props;
    const inputNewValue =
      inputValue !== null && inputValue !== undefined && !isNaN(inputValue)
        ? inputValue.toString()
        : '';
    if (this.inputHasNewValue(inputNewValue, key)) {
      const newValue = value;
      if (!isNaN(inputValue)) {
        newValue[key] = inputNewValue;
      }
      if (onChange) {
        onChange(newValue, inputNewValue, key);
      }
      this.focusKey = key;
      this.add =
        inputValue !== null && inputValue !== undefined && !isNaN(inputValue);
    }
  };

  componentDidUpdate() {
    const { value, length, name, selectedPinField } = this.props;
    const key = value.join('').length;
    const inputValue = value[key - 1];
    this.add =
      inputValue !== null &&
      inputValue !== undefined &&
      !isNaN(inputValue);
    const emptyFieldIndex = value.findIndex((item) => item === "");
    this.focusKey =
      emptyFieldIndex !== undefined &&
      emptyFieldIndex !== null &&
      emptyFieldIndex > -1 ?
      emptyFieldIndex : this.focusKey;
    if (name === selectedPinField && (key > 0 && key < length || emptyFieldIndex > -1)) {
      let inputFocusKey = 0;
      if (emptyFieldIndex > -1) {
        inputFocusKey = emptyFieldIndex;
      } else {
        inputFocusKey = this.add ? this.focusKey + 1 : this.focusKey - 1;
      }
      if (
        Object.prototype.hasOwnProperty.call(this.inputsRef, inputFocusKey) &&
        this.inputsRef[inputFocusKey]
      ) {
        this.inputsRef[inputFocusKey].focus();
      }
    }
  }

  onKeyDown = (evt: SyntheticKeyboardEvent<EventTarget>, inputKey: string) => {
    const { value, onChange } = this.props;
    const { decimalSeparator, groupSeparator } = BigNumber.config().FORMAT;
    const { key, target } = evt;
    const control: { blur?: Function, focus?: Function } = target;
    const focusKey = parseInt(inputKey, 10);
    const nextFieldFocusKey = focusKey + 1;
    const nextInputField = this.inputsRef[nextFieldFocusKey];
    const isSeparator = key === decimalSeparator || key === groupSeparator;
    const isBackspace = key === 'Backspace';
    const inputNewValue = this.inputsRef[focusKey].props.value;
    const fieldIsEmpty = this.inputsRef[focusKey]
      ? !inputNewValue
      : false;

    if (isSeparator) {
      this.handleSeparatorInput(nextInputField, control);
    }

    if (isBackspace && fieldIsEmpty) {
      if (onChange) {
        onChange(value, inputNewValue, inputKey);
      }
      this.focusKey = inputKey;
      this.add = false;
    }
  };

  handleSeparatorInput = (nextInputField: Field, control: { blur?: Function, focus?: Function }) => {
    if (nextInputField && nextInputField.focus) {
      nextInputField.focus();
    }
    if (control && control.blur) {
      control.blur();
      setTimeout(() => {
        if (control && control.focus) {
          control.focus();
        }
      }, 0);
    }
  };

  generatePinCodeInput = () => {
    const {
      id,
      name,
      type,
      autoFocus,
      length,
      error,
      value,
    } = this.props;

    const pinCodeClasses = classNames([
      styles.pinCode,
      error ? styles.error : null,
    ]);

    return (
      <div className={styles.pinCodeInput}>
        {map(Array(length).fill(), (action, index) => {
          return (
            <NumericInput
              ref={(input) => {
                if (
                  !Object.prototype.hasOwnProperty.call(
                    this.inputsRef,
                    index
                  ) ||
                  this.inputsRef[index] !== input
                )
                  this.inputsRef[index] = input;
              }}
              id={id + index}
              name={name}
              type={type}
              className={pinCodeClasses}
              label={null}
              key={index}
              skin={InputSkin}
              onChange={(number) => this.onChange(number, index)}
              onKeyDown={(event) => this.onKeyDown(event, index)}
              value={value ? value[index] : undefined}
              autoFocus={autoFocus && index === 0}
              allowSigns={false}
            />
          );
        })}
      </div>
    );
  };

  render() {
    const { label, error } = this.props;

    const pinCode = this.generatePinCodeInput();

    return (
      <div className={styles.component} role="button">
        <label htmlFor="firstName" className="SimpleFormField_label">
          {label}
        </label>
        {error ? (
          <PopOver
            content={error}
            placement="bottom"
            themeVariables={{
              '--rp-pop-over-bg-color': 'var(--theme-color-error)',
            }}
          >
            {pinCode}
          </PopOver>
        ) : (
          <>{pinCode}</>
        )}
      </div>
    );
  }
}
