// @flow
import React, { Component } from 'react';
import { map } from 'lodash';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import { Tooltip } from 'react-polymorph/lib/components/Tooltip';
import { TooltipSkin } from 'react-polymorph/lib/skins/simple/TooltipSkin';
import classNames from 'classnames';
import tooltipStyles from './FormFieldSkinTooltip-tooltip.scss';
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
  value: number,
  error: string | null,
}>;

export default class PinCode extends Component<Props> {
  static defaultProps = {
    length: 4,
    disabled: false,
  };

  inputsRef = [];
  focusKey = 0;

  onChange = (inputValue: ?number, key: number) => {
    const { value, onChange } = this.props;

    let newValue = value.toString().split('');
    newValue[key] = inputValue ? inputValue.toString() : '';
    newValue = newValue.join('') !== '' ? parseInt(newValue.join(''), 10) : '';

    if (onChange) {
      onChange(newValue);
    }
  };

  componentDidUpdate() {
    const { value, length } = this.props;
    const key = value.toString().length;
    if (key > 0 && key < length) {
      const inputFocusKey = this.focusKey >= key ? key - 1 : key;
      if (
        Object.prototype.hasOwnProperty.call(this.inputsRef, inputFocusKey) &&
        this.inputsRef[inputFocusKey]
      )
        this.inputsRef[inputFocusKey].focus();
      this.focusKey = key;
    }
    if (key === 0) this.focusKey = 0;
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
        {map(Array(length).fill(), (action, key) => {
          const inputValue = value ? value.toString().split('') : undefined;
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
              value={inputValue ? inputValue[key] : undefined}
              autoFocus={autoFocus && key === 0}
              disabled={
                disabled ||
                (key !== 0 &&
                  (!inputValue ||
                    !Object.prototype.hasOwnProperty.call(inputValue, key - 1)))
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
          <Tooltip
            skin={TooltipSkin}
            themeOverrides={tooltipStyles}
            tip={error}
            key="tooltip"
            className={styles.tooltip}
            isOpeningUpward={false}
          >
            {pinCode}
          </Tooltip>
        ) : (
          <>{pinCode}</>
        )}
      </div>
    );
  }
}
