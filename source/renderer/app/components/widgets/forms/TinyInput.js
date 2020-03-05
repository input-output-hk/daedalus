// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import { Input } from 'react-polymorph/lib/components/Input';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import styles from './TinyInput.scss';

type Props = $Exact<{
  autoFocus: boolean,
  innerLabelPrefix?: string,
  innerLabelSuffix?: string,
  innerValue?: Node,
  onKeyPress?: Function,
  onSubmit?: Function,
  type?: string,
  useReadMode?: boolean,
}>;

type State = {
  isEditMode: boolean,
};

export default class TinyInput extends Component<Props, State> {
  state = {
    isEditMode: false,
  };

  setEditMode = (isEditMode: boolean) => this.setState({ isEditMode });

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
        tabIndex={-1}
      >
        {useReadMode && !isEditMode && (
          <div className={styles.contentInReadMode}>
            <span className={styles.innerLabelPrefix}>{innerLabelPrefix}</span>
            <span className={styles.innerValue}>{innerValue}</span>
            <span className={styles.innerLabelSuffix}>{innerLabelSuffix}</span>
          </div>
        )}
        {(!useReadMode || isEditMode) && (
          <>
            {type === 'number' ? (
              <NumericInput
                themeId={IDENTIFIERS.INPUT}
                skin={InputSkin}
                {...restProps}
                autoFocus={autoFocus}
                onKeyPress={this.onKeyPress}
              />
            ) : (
              <Input
                themeId={IDENTIFIERS.INPUT}
                skin={InputSkin}
                {...restProps}
                autoFocus={useReadMode ? true : autoFocus}
                onKeyPress={this.onKeyPress}
                type={type}
              />
            )}
          </>
        )}
      </div>
    );
  }
}
