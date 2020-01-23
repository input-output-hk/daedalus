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
  onBlur?: Function,
  onInput?: Function,
  onPaste?: Function,
  onKeyPress?: Function,
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
    const { notNegative, type } = this.props;
    const numberRegex = new RegExp(/^-?\d*\.?\d*$/);
    const notNegativeNumberRegex = new RegExp(/^\d*\.?\d*$/);

    if (type !== 'number') {
      return true;
    }

    if (notNegative) {
      return notNegativeNumberRegex.test(value);
    }

    return numberRegex.test(value);
  };

  onInput = (evt: SyntheticInputEvent<EventTarget>) => {
    const { type, onInput } = this.props;
    const { prevValue } = this.state;

    if (type === 'number' && evt.target.value === '.') {
      evt.target.value = '0.';
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
    const { onKeyPress } = this.props;
    const { charCode } = evt;

    if (charCode === 13) {
      this.setEditMode(false);
    }

    if (onKeyPress) {
      onKeyPress(evt);
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
