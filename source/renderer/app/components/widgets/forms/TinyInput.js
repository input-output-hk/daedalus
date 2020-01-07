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
  onBlur?: Function,
  onChange?: Function,
  onFocus?: Function,
  onKeyPress?: Function,
  placeholder?: string,
  readOnly: boolean,
  setError?: Function,
  selectedOption?: any,
  selectionRenderer?: Function,
  skin?: ComponentType<any>,
  theme: ?Object, // will take precedence over theme in context if passed
  themeId: string,
  themeOverrides: Object,
  useReadMode?: boolean,
  value: string,
};

type State = {
  isEditMode: boolean,
};

export default class TinyInput extends Component<Props, State> {
  state = {
    isEditMode: false,
  };

  setEditMode = (isEditMode: boolean) => this.setState({ isEditMode });

  render() {
    const {
      autoFocus,
      innerLabelPrefix,
      innerLabelSuffix,
      innerValue,
      useReadMode,
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
          />
        )}
      </div>
    );
    /* eslint-enable */
  }
}
