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
  contentInReadMode?: Element<any>,
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

type State = {
  isEditMode: boolean,
};

export default class TinyInput extends Component<Props, State> {
  state = {
    isEditMode: false,
  };

  setEditMode = (isEditMode: boolean) => this.setState({ isEditMode });

  render() {
    const { contentInReadMode, ...restProps } = this.props;
    const { isEditMode } = this.state;
    const editPanel = (
      <Input
        themeId={IDENTIFIERS.INPUT}
        skin={InputSkin}
        {...restProps}
        autoFocus
      />
    );

    if (!contentInReadMode) {
      return <div className={styles.component}>{editPanel}</div>;
    }

    if (isEditMode) {
      return (
        <div
          className={styles.component}
          onBlur={() => this.setEditMode(false)}
        >
          {editPanel}
        </div>
      );
    }

    return (
      <div className={styles.component} onClick={() => this.setEditMode(true)}>
        {contentInReadMode}
      </div>
    );
  }
}
