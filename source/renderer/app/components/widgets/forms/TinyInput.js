// @flow
import React, { Component } from 'react';
// $FlowFixMe
import type { ComponentType, Element } from 'react';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import styles from './TinyInput.scss';

type Props = {
  autoFocus: boolean,
  className?: ?string,
  disabled?: boolean,
  error: string | Element<any>,
  innerLabelPrefix?: string,
  innerLabelSuffix?: string,
  innerValue?: string,
  label?: string | Element<any>,
  maxLength?: number,
  minLength?: number,
  notNegative?: boolean,
  onBlur?: Function,
  onChange?: Function,
  onFocus?: Function,
  onInput?: Function,
  onKeyPress?: Function,
  onPaste?: Function,
  placeholder?: string,
  readOnly: boolean,
  setError?: Function,
  selectedOption?: any,
  selectionRenderer?: Function,
  skin?: ComponentType<any>,
  theme: ?Object, // will take precedence over theme in context if passed
  themeId: string,
  themeOverrides: Object,
  type?: string,
  useReadMode?: boolean,
  value: string,
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
    const { notNegative, type } = this.props;
    const numberRegex = new RegExp(/^-?\d*\.?\d*$/);
    const notNegativeNumberRegex = new RegExp(/^\d*\.?\d*$/);
    let result = null;

    if (type !== 'number') {
      return true;
    }

    if (notNegative) {
      result = notNegativeNumberRegex.test(value);
    } else {
      result = numberRegex.test(value);
    }

    if (result) {
      if (value !== '.' && Number(value).toFixed(2).length > 19) {
        return false;
      }
    }

    return result;
  };

  onInput = (evt: any) => {
    const { onInput } = this.props;
    const { prevValue } = this.state;
    const { value } = evt.target;

    if (this.validate(value)) {
      this.setState({ prevValue: value });
      if (onInput) {
        onInput(evt);
      }
    } else {
      evt.target.value = prevValue;
    }
  };

  onPaste = (evt: any) => {
    const { onPaste } = this.props;
    const value = evt.clipboardData.getData('text/plain');

    if (this.validate(value)) {
      this.setState({ prevValue: value });
      if (onPaste) {
        onPaste(evt);
      }
    } else {
      evt.preventDefault();
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

    /* eslint-disable */
    return (
      <div
        className={styles.component}
        onClick={() => this.setEditMode(true)}
        onBlur={() => this.setEditMode(false)}
        role="contentinfo"
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
            type={type === 'number' ? 'string' : type}
          />
        )}
      </div>
    );
    /* eslint-enable */
  }
}
