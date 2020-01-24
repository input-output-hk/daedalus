// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import styles from './TinyInput.scss';

type Props = {
  autoFocus: boolean,
  innerLabelPrefix?: string,
  innerLabelSuffix?: string,
  innerValue?: Node,
  notNegative?: boolean,
  digitCountAfterDecimalPoint?: number,
  onBlur?: Function,
  onInput?: Function,
  onPaste?: Function,
  onKeyPress?: Function,
  onSubmit?: Function,
  type?: string,
  useReadMode?: boolean,
};

type State = {
  isEditMode: boolean,
  prevValue: string,
};

export default class TinyInput extends Component<Props, State> {
  state = {
    isEditMode: false,
    prevValue: '',
  };

  setEditMode = (isEditMode: boolean) => this.setState({ isEditMode });

  validate = (value: string) => {
    const { notNegative, type, digitCountAfterDecimalPoint } = this.props;
    const numberRegex = new RegExp(/^-?\d*\.?\d*$/);
    const notNegativeNumberRegex = new RegExp(/^\d*\.?\d*$/);
    const numValue = Number(value);
    const decimalPointPosition = value.indexOf('.');
    let result = null;

    if (type !== 'number') {
      return true;
    }

    if (notNegative) {
      result = notNegativeNumberRegex.test(value);
    } else {
      result = numberRegex.test(value);
    }

    if (
      result &&
      !Number.isNaN(numValue) &&
      decimalPointPosition > -1 &&
      digitCountAfterDecimalPoint &&
      value.length - decimalPointPosition - 1 > digitCountAfterDecimalPoint
    ) {
      result = false;
    }

    return result;
  };

  onInput = (evt: SyntheticInputEvent<EventTarget>) => {
    const { type, onInput } = this.props;
    const { prevValue } = this.state;
    const onlyZerosRegex = new RegExp(/^00+$/);

    if (type === 'number') {
      if (evt.target.value === '.') {
        evt.target.value = '0.';
      }
      if (onlyZerosRegex.test(evt.target.value)) {
        evt.target.value = `0.${evt.target.value.substring(1)}`;
      }
    }

    const { value } = evt.target;

    if (this.validate(value)) {
      this.setState({ prevValue: value });
    } else {
      evt.target.value = prevValue;
    }

    if (onInput) {
      onInput(evt);
    }
  };

  onPaste = (evt: SyntheticClipboardEvent<EventTarget>) => {
    const { onPaste } = this.props;
    const value = evt.clipboardData.getData('text/plain');

    if (this.validate(value)) {
      this.setState({ prevValue: value });
    } else {
      evt.preventDefault();
    }

    if (onPaste) {
      onPaste(evt);
    }
  };

  onKeyPress = (evt: SyntheticKeyboardEvent<EventTarget>) => {
    const { onKeyPress, onSubmit } = this.props;
    const { charCode } = evt;
    const control: { blur?: Function } = evt.target;

    if (onKeyPress) {
      onKeyPress(evt);
    }

    if (charCode === 13 && control.blur) {
      control.blur();
      if (onSubmit) {
        onSubmit();
      }
    }
  };

  render() {
    const {
      autoFocus,
      innerLabelPrefix,
      innerLabelSuffix,
      innerValue,
      useReadMode,
      type,
      ...restProps
    } = this.props;
    const { isEditMode } = this.state;

    return (
      <div
        className={styles.component}
        onFocus={() => this.setEditMode(true)}
        onBlur={() => this.setEditMode(false)}
        role="button"
        tabIndex={0}
      >
        {useReadMode && !isEditMode && (
          <div className={styles.contentInReadMode}>
            <span className={styles.innerLabelPrefix}>{innerLabelPrefix}</span>
            <span className={styles.innerValue}>{innerValue}</span>
            <span className={styles.innerLabelSuffix}>{innerLabelSuffix}</span>
          </div>
        )}
        {(!useReadMode || isEditMode) && (
          <Input
            themeId={IDENTIFIERS.INPUT}
            skin={InputSkin}
            {...restProps}
            autoFocus={useReadMode ? true : autoFocus}
            onInput={this.onInput}
            onPaste={this.onPaste}
            onKeyPress={this.onKeyPress}
            type={type === 'number' ? 'string' : type}
          />
        )}
      </div>
    );
  }
}
