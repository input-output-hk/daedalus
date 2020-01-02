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
  value: string,
};

export default class TinyButton extends Component<Props> {
  render() {
    return (
      <Input
        className={styles.component}
        themeId={IDENTIFIERS.INPUT}
        skin={InputSkin}
        {...this.props}
      />
    );
  }
}
