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
  focusKeyChanged: boolean,
  focusIsUpdated: boolean,
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
    focusKeyChanged: false,
    focusIsUpdated: false,
  };

  hasInputNewValue = (inputNewValue: string, key: number) => {
    const { value } = this.props;
    const valueIsEmpty =
      !value[key] || value[key] === '' || inputNewValue === '';
    const valueHasChanged =
      inputNewValue &&
      inputNewValue.length === 1 &&
      value[key] !== inputNewValue;
    return valueIsEmpty || valueHasChanged;
  };

  isInputValueSelected = (input: Field) => {
    const { inputElement } = input;
    const { current } = inputElement;
    return current.selectionStart !== current.selectionEnd;
  };

  isInputFieldDisabled = (
    index: number,
    value: Array<string>,
    disabled: boolean
  ) => {
    return (
      disabled ||
      (index !== 0 && (!value || !value[index - 1])) ||
      (index > 1 && index <= 4 && !value[index - 2]) ||
      (index === 3 && value[index - 3] === '')
    );
  };

  onChange = (inputValue: ?number | ?string, key: number) => {
    const { value, onChange } = this.props;
    const { isBackSpace } = this.state;
    const inputNewValue =
      inputValue !== null && inputValue !== undefined && !isNaN(inputValue)
        ? inputValue.toString()
        : '';
    if (this.hasInputNewValue(inputNewValue, key)) {
      const newValue = value;
      if (!isNaN(inputValue)) {
        // Recheck if user pressed backspace and moved cursor to previous input field which has value
        if (
          isBackSpace &&
          newValue[key] !== '' &&
          inputNewValue === '' &&
          this.focusKey !== key
        ) {
          // Set old value to field because it was previously deleted while shifting focus
          newValue[key] = value[key];
          // Calculate new field key for focus
          const focusKey =
            this.inputsRef[key] &&
            this.inputsRef[key].inputElement.current.selectionStart === 0 &&
            key > 2 &&
            key < this.inputsRef.length
              ? key - 1
              : key;
          // Delay focus to the same field while waiting for the validation to pass
          setTimeout(() => {
            const inputFieldRef = this.inputsRef[focusKey];
            if (inputFieldRef && inputFieldRef.inputElement) {
              this.setFocusOnField(inputFieldRef);
              this.setState({ focusKeyChanged: false, focusIsUpdated: true });
            }
          }, 0);
        } else {
          // Set new value to input field when focus was not shifted to previous field
          newValue[key] = inputNewValue;
          this.setState({
            isBackSpace: false,
            focusKeyChanged: false,
            focusIsUpdated: false,
          });
        }
      }
      if (onChange) {
        // Send new updated value to onChange event
        onChange(newValue, inputNewValue, key);
      }
      // Update focus key
      this.focusKey = key;
      // Recheck if user is adding or deleting value
      this.add =
        inputValue !== null && inputValue !== undefined && !isNaN(inputValue);
    }
  };

  componentDidUpdate() {
    const { value, length, name, selectedPinField } = this.props;
    const { isBackSpace, focusKeyChanged, focusIsUpdated } = this.state;
    const key = value.join('').length;
    const inputValue = value[key - 1];
    this.add = this.fromBackspace
      ? false
      : inputValue !== null && inputValue !== undefined && !isNaN(inputValue);
    // Find index of first empty input field element
    const emptyFieldIndex = value.findIndex((item) => item === '');
    // Update focus key index
    this.focusKey =
      emptyFieldIndex !== undefined &&
      emptyFieldIndex !== null &&
      emptyFieldIndex > -1
        ? emptyFieldIndex
        : this.focusKey;
    const focusKey = parseInt(this.focusKey, 10);
    if (
      name === selectedPinField &&
      ((!focusIsUpdated && key > 0 && key < length) ||
        emptyFieldIndex > -1 ||
        focusKeyChanged)
    ) {
      let inputFocusKey = 0;
      // Calculate new input focus key based on a action - delete/add of field value
      if (emptyFieldIndex > -1 && !this.fromBackspace) {
        inputFocusKey = emptyFieldIndex;
      } else {
        inputFocusKey = this.add ? focusKey + 1 : focusKey - 1;
      }
      // Grab current focused element reference
      const inputElementRef = this.inputsRef[inputFocusKey];
      if (
        Object.prototype.hasOwnProperty.call(this.inputsRef, inputFocusKey) &&
        inputElementRef
      ) {
        this.fromBackspace = false;
        // Check if backspace was pressed and focus key is different
        // Then re-focus field and move cursor pointer in front (before) of field value
        if (
          isBackSpace &&
          inputElementRef.inputElement &&
          emptyFieldIndex !== inputFocusKey
        ) {
          this.setFocusOnField(inputElementRef);
        } else if (!isBackSpace) {
          // If new value was added to already empty field, just re-focus to the same field
          inputElementRef.focus();
        }
      }
    }
  }

  onKeyDown = (evt: SyntheticKeyboardEvent<EventTarget>, inputKey: number) => {
    const { decimalSeparator, groupSeparator } = BigNumber.config().FORMAT;
    const { key, target } = evt;
    const control: { blur?: Function, focus?: Function } = target;
    const nextFieldFocusKey = inputKey + 1;
    const nextInputField = this.inputsRef[nextFieldFocusKey];
    // Recheck if input value is a separator value
    const isSeparator = key === decimalSeparator || key === groupSeparator;
    // Recheck if input value is a backspace value
    const isBackSpace = key === 'Backspace';
    // Get input field new value
    const inputNewValue = this.inputsRef[inputKey]
      ? this.inputsRef[inputKey].props.value
      : null;
    // Recheck if field is empty
    const fieldIsEmpty = this.inputsRef[inputKey] ? !inputNewValue : false;
    // Get cursor pointer position from input field
    const selectionStart = this.inputsRef[inputKey]
      ? this.inputsRef[inputKey].inputElement.current.selectionStart
      : 0;
    const selectionEnd = this.inputsRef[inputKey]
      ? this.inputsRef[inputKey].inputElement.current.selectionEnd
      : 0;
    const isEntrySelected = selectionStart !== selectionEnd;
    if (isSeparator) {
      this.handleSeparatorInput(nextInputField, control);
    }
    this.handleBackspaceClick(
      inputNewValue,
      isBackSpace,
      fieldIsEmpty,
      inputKey,
      isEntrySelected
    );
  };

  handleBackspaceClick = (
    inputNewValue: string | null,
    isBackSpace: boolean,
    fieldIsEmpty: boolean,
    inputKey: number,
    isEntrySelected: boolean
  ) => {
    const { value, onChange } = this.props;
    const { focusKeyChanged } = this.state;
    let focusKeyUpdated = false;
    if (isBackSpace && fieldIsEmpty && !isEntrySelected) {
      if (onChange) {
        // Handle specific case when user pressed backspace and field is empty
        // or cursor pointer position was in front of the value
        if (this.isInputValueSelected(this.inputsRef[inputKey])) {
          // Remove value from input field
          value[inputKey] = '';
        } else {
          focusKeyUpdated = true;
        }
        this.setState({ isBackSpace, focusKeyChanged: focusKeyUpdated });
        // Call onChange function to validate new value in focused input field
        onChange(value, inputNewValue, inputKey);
      }
      this.focusKey = focusKeyChanged ? inputKey - 1 : inputKey;
      this.add = false;
      this.fromBackspace = true;
    } else {
      this.focusKey = inputKey;
      this.fromBackspace = false;
    }
  };

  setFocusOnField = (inputFieldRef: { focus: ?Function, props: ?Object, inputElement: ?Object }) => {
    const { focus, props, inputElement } = inputFieldRef;
    if (focus) focus();
    if (inputElement && props && props.value) {
      inputElement.current.selectionStart = 0;
      inputElement.current.selectionEnd = 1;
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
              disabled={this.isInputFieldDisabled(index, value, disabled)}
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
