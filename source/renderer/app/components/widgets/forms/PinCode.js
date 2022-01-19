// @flow
import React, { Component } from 'react';
import { intlShape } from 'react-intl';
import { map, isNaN } from 'lodash';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classNames from 'classnames';
import BigNumber from 'bignumber.js';
import type { Field } from 'mobx-react-form';
import SVGInline from 'react-svg-inline';
import { Button } from 'react-polymorph/lib/components/Button';
import styles from './PinCode.scss';
import { VOTING_REGISTRATION_PIN_CODE_LENGTH } from '../../../config/votingConfig';
import revealKeyImage from '../../../assets/images/reveal-key.inline.svg';
import hideKeyImage from '../../../assets/images/hide-key.inline.svg';
import globalMessages from '../../../i18n/global-messages';

type Props = $Exact<{
  id: string,
  name: string,
  type: string,
  autoFocus: boolean,
  onChange?: Function,
  onResetValues: Function,
  onShowHideValues: Function,
  onUpdateFieldDisabledStates: Function,
  label: string,
  resetLabel: string,
  length: number,
  disabled: boolean,
  value: Array<string>,
  error: string | null,
  selectedPinField: ?string,
  isResetButtonDisabled: boolean,
  pinCodesVisible: boolean,
  onTabKey: Function,
  sectionToFocus: ?string,
  isTabClicked: boolean,
  pinFieldDisabledStates: Array<boolean>,
  repeatPinFieldDisabledStates: Array<boolean>,
}>;

type State = {
  isBackSpace: boolean,
  focusKeyChanged: boolean,
  focusIsUpdated: boolean,
};

export default class PinCode extends Component<Props, State> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

  static defaultProps = {
    length: VOTING_REGISTRATION_PIN_CODE_LENGTH,
    disabled: false,
    value: [],
  };

  inputsRef = [];
  focusKey = 0;
  isAddingNewValue = false;
  fromBackspace = false;

  state = {
    isBackSpace: false,
    focusKeyChanged: false,
    focusIsUpdated: false,
  };

  valueHasChanged = (inputNewValue: string, key: number) => {
    const { value } = this.props;
    const emptyOrUnchangedValue = !value[key] || !inputNewValue;
    const valueHasChanged =
      inputNewValue &&
      inputNewValue.length === 1 &&
      value[key] !== inputNewValue;
    return emptyOrUnchangedValue || valueHasChanged;
  };

  isInputValueSelected = (input: Field) => {
    const { inputElement } = input;
    const { current } = inputElement;
    return current.selectionStart !== current.selectionEnd;
  };

  onChange = (inputValue: ?number, key: number) => {
    const { value, onChange, onUpdateFieldDisabledStates, length } = this.props;
    const { isBackSpace } = this.state;
    const inputNewValue =
      inputValue && !isNaN(inputValue) ? inputValue.toString() : '';
    if (this.valueHasChanged(inputNewValue, key)) {
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
              setTimeout(() => {
                this.setFocusOnField(inputFieldRef);
              }, 0);
              this.setState({
                focusKeyChanged: false,
                focusIsUpdated: true,
              });
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
        onChange(newValue);
      }
      // Update focus key
      this.focusKey = key;
      // Recheck if user is adding or deleting value
      this.isAddingNewValue = inputValue && !isNaN(inputValue);
      const shouldUpdateFieldDisabledStates =
        inputNewValue !== '' && newValue[key] === inputNewValue;
      if (onUpdateFieldDisabledStates && shouldUpdateFieldDisabledStates) {
        const nextKeyForward = key + 1 <= value.length ? key + 1 : null;
        const nextKeyBackward = key - 1 >= 0 ? key - 1 : null;
        onUpdateFieldDisabledStates(
          key,
          this.isAddingNewValue ? nextKeyForward : nextKeyBackward,
          false,
          key === length - 1 && nextKeyForward && nextKeyForward >= length
        );
      }
    }
  };

  componentDidMount() {
    const { name } = this.props;
    if (name === 'pinCode') {
      this.setFocusOnField(this.inputsRef[this.focusKey]);
    }
  }

  componentDidUpdate() {
    const {
      value,
      length,
      name,
      selectedPinField,
      sectionToFocus,
      isTabClicked,
      onUpdateFieldDisabledStates,
      pinFieldDisabledStates,
      repeatPinFieldDisabledStates,
    } = this.props;
    const { isBackSpace, focusKeyChanged, focusIsUpdated } = this.state;
    const key = value.join('').length;
    const inputValue = value[key - 1];
    this.isAddingNewValue = this.fromBackspace
      ? false
      : inputValue && !isNaN(inputValue) && !isTabClicked;
    // Find index of first empty input field element
    const emptyFieldIndex = value.length
      ? value.findIndex((item) => item === '')
      : 0;
    // Update focus key index
    this.focusKey =
      emptyFieldIndex !== undefined &&
      emptyFieldIndex !== null &&
      emptyFieldIndex > -1
        ? emptyFieldIndex
        : this.focusKey;
    const focusKey = parseInt(this.focusKey, 10);
    if (
      name === sectionToFocus ||
      (name === selectedPinField &&
        ((!focusIsUpdated && key > 0 && key < length) ||
          emptyFieldIndex > -1 ||
          focusKeyChanged)) ||
      (name !== selectedPinField &&
        (emptyFieldIndex === -1 || emptyFieldIndex === 0) &&
        this.focusKey === 0) ||
      sectionToFocus === 'continueButton'
    ) {
      let inputFocusKey = 0;
      // Calculate new input focus key based on a action - delete/add of field value
      if (emptyFieldIndex > -1 && !this.fromBackspace) {
        inputFocusKey = emptyFieldIndex;
      } else if (name === sectionToFocus) {
        inputFocusKey = focusKey;
      } else if (
        focusKey === 0 &&
        emptyFieldIndex === -1 &&
        !sectionToFocus &&
        name === 'repeatPinCode' &&
        selectedPinField === 'pinCode'
      ) {
        inputFocusKey = focusKey;
      } else if (
        focusKey >= 0 &&
        emptyFieldIndex >= 0 &&
        !sectionToFocus &&
        name === 'repeatPinCode' &&
        name === 'pinCode'
      ) {
        inputFocusKey = emptyFieldIndex;
      } else {
        inputFocusKey = this.isAddingNewValue ? focusKey + 1 : focusKey - 1;
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
        } else if (
          !isBackSpace &&
          name !== sectionToFocus &&
          sectionToFocus !== 'continueButton'
        ) {
          // If new value was added to already empty field, just re-focus to the same field
          this.setFocusOnField(inputElementRef);
        } else if (name === sectionToFocus && !focusKeyChanged) {
          const pinCodeEnabledFields = pinFieldDisabledStates
            ? pinFieldDisabledStates.filter((item) => !item).length
            : 0;
          const repeatPinCodeEnabledFields = repeatPinFieldDisabledStates
            ? repeatPinFieldDisabledStates.filter((item) => !item).length
            : 0;
          if (
            !pinCodeEnabledFields &&
            !repeatPinCodeEnabledFields &&
            onUpdateFieldDisabledStates
          ) {
            onUpdateFieldDisabledStates(inputFocusKey, null);
          }
          setTimeout(() => {
            this.setFocusOnField(inputElementRef);
          }, 0);
        } else if (isBackSpace && focusKeyChanged && name === sectionToFocus) {
          const indexToFocus = this.isAddingNewValue
            ? focusKey + 1
            : focusKey - 1;
          const inputElRef = this.inputsRef[indexToFocus];
          setTimeout(() => {
            this.setFocusOnField(inputElRef);
          }, 0);
        }
      }
    }
  }

  onKeyDown = (evt: SyntheticKeyboardEvent<EventTarget>, inputKey: number) => {
    const { onTabKey } = this.props;
    const { decimalSeparator, groupSeparator } = BigNumber.config().FORMAT;
    const { key, target } = evt;
    const control: { blur?: Function, focus?: Function } = target;
    const nextFieldFocusKey = inputKey + 1;
    const nextInputField = this.inputsRef[nextFieldFocusKey];
    // Recheck if input value is a separator value
    const isSeparator = key === decimalSeparator || key === groupSeparator;
    // Recheck if input value is a backspace value
    const isBackSpace = key === 'Backspace';
    // Recheck if input value is tab value
    const isTab = key === 'Tab';
    // Recheck if input value is left arrow key
    const isArrowKey =
      key === 'ArrowLeft' || key === 'ArrowUp' || key === 'ArrowDown';
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
    if (isSeparator || isArrowKey) {
      this.handleSeparatorInput(nextInputField, control);
    }

    if (isTab && onTabKey) {
      onTabKey();
    }

    if (isBackSpace) {
      this.handleBackspaceClick(
        inputNewValue,
        isBackSpace,
        fieldIsEmpty,
        inputKey,
        isEntrySelected,
        isTab
      );
    }
  };

  handleBackspaceClick = (
    inputNewValue: string | null,
    isBackSpace: boolean,
    fieldIsEmpty: boolean,
    inputKey: number,
    isEntrySelected: boolean,
    isTab: boolean
  ) => {
    const {
      value,
      onChange,
      onTabKey,
      onUpdateFieldDisabledStates,
    } = this.props;
    const { focusKeyChanged } = this.state;
    const inputElRef = this.inputsRef[inputKey];
    let focusKeyUpdated = false;
    if (
      isBackSpace &&
      ((fieldIsEmpty && !isEntrySelected) ||
        (!fieldIsEmpty &&
          inputElRef &&
          inputElRef.inputElement.current.selectionStart === 0))
    ) {
      if (onChange) {
        // Handle specific case when user pressed backspace and field is empty
        // or cursor pointer position was in front of the value
        if (this.isInputValueSelected(inputElRef)) {
          // Remove value from input field
          value[inputKey] = '';
        } else {
          focusKeyUpdated = true;
        }
        this.setState({
          isBackSpace,
          focusKeyChanged: focusKeyUpdated,
        });
        // Call onChange function to validate new value in focused input field
        onChange(value);
      }
      this.focusKey = focusKeyChanged ? inputKey - 1 : inputKey;
      this.isAddingNewValue = false;
      this.fromBackspace = true;
      const shouldUpdateFieldDisabledStates =
        !inputNewValue && !value[inputKey] && inputKey > 0;
      if (onUpdateFieldDisabledStates && shouldUpdateFieldDisabledStates) {
        const nextKeyForward =
          this.focusKey + 1 <= value.length ? this.focusKey + 1 : null;
        const nextKeyBackward =
          this.focusKey - 1 >= 0 ? this.focusKey - 1 : null;
        onUpdateFieldDisabledStates(
          this.focusKey,
          this.isAddingNewValue ? nextKeyForward : nextKeyBackward
        );
      }
    } else {
      this.focusKey = inputKey;
      this.fromBackspace = false;
      if (isTab && onTabKey) {
        if (onChange) {
          onChange(value, isTab);
        }
      }
    }
  };

  setFocusOnField = (
    inputFieldRef: {
      focus: ?Function,
      props: { value: string },
      inputElement: {
        current: { selectionStart: number, selectionEnd: number },
      },
    } | null
  ) => {
    if (inputFieldRef) {
      const { focus, props, inputElement } = inputFieldRef;
      if (focus) focus();
      if (inputElement && props.value) {
        inputElement.current.selectionStart = 1;
        inputElement.current.selectionEnd = 1;
      }
    }
  };

  handleSeparatorInput = (
    nextInputField: Field,
    control: { blur?: Function, focus?: Function }
  ) => {
    if (nextInputField) {
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

  handlePinCodeSectionClick = (event: MouseEvent) => {
    const { target } = event;
    if (!(target instanceof HTMLInputElement) || target.disabled) {
      const {
        value,
        onUpdateFieldDisabledStates,
        length,
        pinFieldDisabledStates,
        repeatPinFieldDisabledStates,
      } = this.props;
      const fieldStates =
        pinFieldDisabledStates || repeatPinFieldDisabledStates;
      const activeField = fieldStates
        .map((item, index) => {
          return { value: item, index };
        })
        .find((item) => !item.value);
      const fieldKey = activeField ? activeField.index : null;
      const hasAvailableField = value.filter((item) => item).length < length;
      const allDisabledStates =
        !fieldKey && !hasAvailableField && value.length === length;
      let fieldToFocusIndex = null;
      if (activeField || allDisabledStates) {
        if (onUpdateFieldDisabledStates) {
          let currentIndex = null;
          let nextIndex = null;
          if (allDisabledStates && !activeField) {
            currentIndex = length - 1;
            nextIndex = null;
            fieldToFocusIndex = length - 1;
          } else if (!allDisabledStates && activeField) {
            currentIndex =
              (hasAvailableField && fieldKey === value.length) ||
              (!hasAvailableField && fieldKey === length - 1)
                ? null
                : fieldKey;
            nextIndex =
              hasAvailableField && fieldKey !== value.length
                ? value.length
                : null;
            fieldToFocusIndex =
              hasAvailableField && fieldKey && fieldKey <= this.focusKey
                ? fieldKey + 1
                : fieldKey;
          }
          onUpdateFieldDisabledStates(
            currentIndex,
            nextIndex,
            !pinFieldDisabledStates ? 'pinCode' : 'repeatPinCode'
          );
        }
        if (fieldToFocusIndex) {
          const fieldToFocus = this.inputsRef[fieldToFocusIndex];
          if (fieldToFocus) {
            setTimeout(() => {
              this.setFocusOnField(fieldToFocus);
            }, 500);
          }
        }
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
      pinCodesVisible,
      pinFieldDisabledStates,
      repeatPinFieldDisabledStates,
    } = this.props;

    const pinCodeClasses = classNames([
      styles.pinCode,
      error ? styles.error : null,
    ]);

    const disabledStates =
      name === 'pinCode'
        ? pinFieldDisabledStates
        : repeatPinFieldDisabledStates;

    return (
      <div
        className={styles.pinCodeInput}
        onClick={this.handlePinCodeSectionClick}
      >
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
              type={pinCodesVisible ? 'text' : type}
              className={pinCodeClasses}
              label={null}
              key={index}
              skin={InputSkin}
              onChange={(number) => this.onChange(number, index)}
              onKeyDown={(event) => this.onKeyDown(event, index)}
              value={value ? value[index] : undefined}
              autoFocus={autoFocus && index === 0}
              allowSigns={false}
              disabled={disabled || disabledStates[index]}
            />
          );
        })}
      </div>
    );
  };

  render() {
    const {
      label,
      resetLabel,
      error,
      isResetButtonDisabled,
      onResetValues,
      name,
      value,
      length,
      onShowHideValues,
      pinCodesVisible,
    } = this.props;

    const { intl } = this.context;

    const toggleButtonTooltip = intl.formatMessage(
      globalMessages[pinCodesVisible ? 'hide' : 'reveal']
    );

    const revealHidePinCodesStyles = classNames([
      styles.pinCodeButton,
      pinCodesVisible ? styles.hideButton : styles.revealButton,
      'flat',
    ]);

    const clearPinCodesStyles = classNames([styles.clearPinCodeButton, 'flat']);

    const pinCode = this.generatePinCodeInput();

    return (
      <div className={styles.component} role="button">
        <div className={styles.labelContainer}>
          <label htmlFor="firstName" className="SimpleFormField_label">
            {label}
          </label>
          <div className={styles.buttonsContainer}>
            <PopOver content={resetLabel}>
              <Button
                className={clearPinCodesStyles}
                onClick={() => {
                  if (onResetValues) {
                    const focusKey =
                      value.length === length ? value.length - 1 : value.length;
                    onResetValues(name, focusKey);
                  }
                  setTimeout(() => {
                    this.setFocusOnField(this.inputsRef[0]);
                  }, 0);
                }}
                label={resetLabel}
                disabled={isResetButtonDisabled}
                tabIndex={-1}
              />
            </PopOver>
            <PopOver content={toggleButtonTooltip}>
              <Button
                className={revealHidePinCodesStyles}
                label={
                  pinCodesVisible ? (
                    <SVGInline svg={hideKeyImage} />
                  ) : (
                    <SVGInline svg={revealKeyImage} />
                  )
                }
                onClick={() => {
                  if (onShowHideValues) {
                    onShowHideValues();
                  }
                  setTimeout(() => {
                    this.setFocusOnField(this.inputsRef[this.focusKey]);
                  }, 0);
                }}
                tabIndex={-1}
              />
            </PopOver>
          </div>
        </div>
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
