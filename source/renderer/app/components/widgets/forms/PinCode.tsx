import React, { Component } from 'react';
import { map } from 'lodash';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classNames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './PinCode.scss' or its corresp... Remove this comment to see the full error message
import styles from './PinCode.scss';

type Props = {
  id: string;
  name: string;
  type: string;
  autoFocus: boolean;
  onChange?: (...args: Array<any>) => any;
  label: string;
  length: number;
  disabled: boolean;
  value: Array<string>;
  error: string | null;
};
export default class PinCode extends Component<Props> {
  static defaultProps = {
    length: 4,
    disabled: false,
    value: [],
  };
  inputsRef = [];
  focusKey = 0;
  add = false;
  onChange = (inputValue: number | null | undefined, key: number) => {
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
    const pinCodeClasses = classNames([
      styles.pinCode,
      error ? styles.error : null,
    ]);
    return (
      <div className={styles.pinCodeInput}>
        {/* @ts-ignore ts-migrate(2554) FIXME: Expected 1-3 arguments, but got 0. */}
        {map(Array(length).fill(), (action, key) => {
          return (
            <NumericInput
              ref={(input) => {
                if (
                  !Object.prototype.hasOwnProperty.call(this.inputsRef, key) ||
                  this.inputsRef[key] !== input
                )
                  this.inputsRef[key] = input;
              }}
              id={id}
              name={name}
              type={type}
              className={pinCodeClasses}
              label={null}
              key={key}
              themeId={IDENTIFIERS.INPUT}
              skin={InputSkin}
              onChange={(number) => this.onChange(number, key)}
              value={value ? value[key] : undefined}
              autoFocus={autoFocus && key === 0}
              disabled={
                disabled ||
                (key !== 0 &&
                  (!value ||
                    !Object.prototype.hasOwnProperty.call(value, key - 1)))
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
