// @flow
import React, { Component } from 'react';
import { map, isNaN } from 'lodash';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classNames from 'classnames';
import BigNumber from 'bignumber.js';
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

  onChange = (inputValue: ?number | ?string, key: number) => {
    const { value, onChange } = this.props;

    const inputNewValue =
      inputValue !== null && inputValue !== undefined && !isNaN(inputValue)
        ? inputValue.toString()
        : '';
    if (
      !value[key] ||
      value[key] === '' ||
      inputNewValue === '' ||
      (inputNewValue &&
        inputNewValue.length === 1 &&
        value[key] !== inputNewValue)
    ) {
      const newValue = value;
      if (!isNaN(inputValue)) {
        newValue[key] = inputNewValue;
      }
      if (onChange) {
        onChange(newValue);
      }
      this.focusKey = key;
      this.add =
        inputValue !== null && inputValue !== undefined && !isNaN(inputValue);
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

  onKeyPress = (evt: SyntheticKeyboardEvent<EventTarget>, inputKey: string) => {
    const { decimalSeparator, groupSeparator } = BigNumber.config().FORMAT;
    const { key, target } = evt;
    const control: { blur?: Function, focus?: Function } = target;
    const focusKey = parseInt(inputKey, 10);
    const nextFieldFocusKey = focusKey + 1;
    const nextInputField = this.inputsRef[nextFieldFocusKey];
    const isSeparator = key === decimalSeparator || key === groupSeparator;
    if (isSeparator) {
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
      disabled,
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
                  !Object.prototype.hasOwnProperty.call(this.inputsRef, index) ||
                  this.inputsRef[index] !== input
                )
                  this.inputsRef[index] = input;
              }}
              id={id}
              name={name}
              type={type}
              className={pinCodeClasses}
              label={null}
              key={index}
              skin={InputSkin}
              onChange={(number) => this.onChange(number, index)}
              onKeyPress={(event) => this.onKeyPress(event, index)}
              value={value ? value[index] : undefined}
              autoFocus={autoFocus}
              allowSigns={false}
              disabled={
                disabled ||
                (index &&
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
