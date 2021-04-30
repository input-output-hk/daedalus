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

type State = {
  isBackSpace: boolean,
};

export default class PinCode extends Component<Props, State> {
  static defaultProps = {
    length: 4,
    disabled: false,
    value: [],
  };

  inputsRef = [];
  focusKey = 0;
  add = false;
  fromBackspace = false;

  state = {
    isBackSpace: false,
  };

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

  inputIsMarked = (input: Field) => {
    const { inputElement } = input;
    const { current } = inputElement;
    return current.selectionStart !== current.selectionEnd;
  };

  onChange = (inputValue: ?number | ?string, key: number) => {
    const { value, onChange } = this.props;
    const { isBackSpace } = this.state;
    const inputNewValue =
      inputValue !== null && inputValue !== undefined && !isNaN(inputValue)
        ? inputValue.toString()
        : '';
    if (this.inputHasNewValue(inputNewValue, key)) {
      const newValue = value;
      if (!isNaN(inputValue))  {
        if (isBackSpace && newValue[key] !== '' && inputNewValue === '' && this.focusKey !== key) {
          newValue[key] = value[key];
          const focusKey = this.inputsRef[key] && this.inputsRef[key].inputElement.current.selectionStart === 0 && (key > 2 && key < this.inputsRef.length) ? key - 1 : key;
          setTimeout(() => {
            const inputFieldRef = this.inputsRef[focusKey];
            if (inputFieldRef && inputFieldRef.inputElement) {
              inputFieldRef.focus();
              inputFieldRef.inputElement.current.select();
            }
          }, 100);
        } else {
          newValue[key] = inputNewValue;
          this.setState({ isBackSpace: false });
        }
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
    const { isBackSpace } = this.state;
    const key = value.join('').length;
    const inputValue = value[key - 1];
    this.add = this.fromBackspace
      ? false
      : inputValue !== null && inputValue !== undefined && !isNaN(inputValue);
    const emptyFieldIndex = value.findIndex((item) => item === '');
    this.focusKey =
      emptyFieldIndex !== undefined &&
      emptyFieldIndex !== null &&
      emptyFieldIndex > -1
        ? emptyFieldIndex
        : this.focusKey;
    const focusKey = parseInt(this.focusKey, 10);
    if (
      name === selectedPinField &&
      ((key > 0 && key < length) || emptyFieldIndex > -1)
    ) {
      let inputFocusKey = 0;
      if (emptyFieldIndex > -1 && !this.fromBackspace) {
        inputFocusKey = emptyFieldIndex;
      } else {
        inputFocusKey = this.add ? focusKey + 1 : focusKey - 1;
      }
      const inputElementRef =  this.inputsRef[inputFocusKey];
      if (
        Object.prototype.hasOwnProperty.call(this.inputsRef, inputFocusKey) &&
        inputElementRef
      ) {
        this.fromBackspace = false;
        if (isBackSpace && inputElementRef.inputElement && emptyFieldIndex !== inputFocusKey) {
          inputElementRef.focus();
          inputElementRef.inputElement.current.select();
        } else if (!isBackSpace) {
          inputElementRef.focus();
        }
      }
    }
  }

  onKeyDown = (evt: SyntheticKeyboardEvent<EventTarget>, inputKey: number) => {
    const { value, onChange } = this.props;
    const { decimalSeparator, groupSeparator } = BigNumber.config().FORMAT;
    const { key, target } = evt;
    const control: { blur?: Function, focus?: Function } = target;
    const nextFieldFocusKey = inputKey + 1;
    const nextInputField = this.inputsRef[nextFieldFocusKey];
    const isSeparator = key === decimalSeparator || key === groupSeparator;
    const isBackSpace = key === 'Backspace';
    const inputNewValue = this.inputsRef[inputKey]
      ? this.inputsRef[inputKey].props.value
      : null;
    const fieldIsEmpty = this.inputsRef[inputKey] ? !inputNewValue : false;
    const cursorPosition = this.inputsRef[inputKey] ? this.inputsRef[inputKey].inputElement.current.selectionStart : 1;
    if (isSeparator) {
      this.handleSeparatorInput(nextInputField, control);
    }
    if (isBackSpace && (fieldIsEmpty || cursorPosition === 0)) {
      if (onChange) {
        if (this.inputIsMarked(this.inputsRef[inputKey])) {
          value[inputKey] = "";
        }
        this.setState({ isBackSpace });
        onChange(value, inputNewValue, inputKey);
      }
      this.focusKey = inputKey;
      this.add = false;
      this.fromBackspace = true;
    } else {
      this.focusKey = inputKey;
      this.fromBackspace = false;
    }
  };

  handleSeparatorInput = (
    nextInputField: Field,
    control: { blur?: Function, focus?: Function }
  ) => {
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
    const { id, name, type, autoFocus, length, error, value, disabled } = this.props;

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
              disabled={
                disabled ||
                (index !== 0 &&
                  (!value ||
                    !value[index - 1]))
              }
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
