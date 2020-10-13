// @flow
import React, { Component } from 'react';
import type { Node } from 'react';
import { find, map } from 'lodash';
import { NumericInput } from 'react-polymorph/lib/components/NumericInput';
import { InputSkin } from 'react-polymorph/lib/skins/simple/InputSkin';
import { IDENTIFIERS } from 'react-polymorph/lib/themes/API';
import styles from './PinCode.scss';

type Props = $Exact<{
  autoFocus: boolean,
  onChange?: Function,
  label: string,
  length: number,
  visible: Boolean,
  value: number,
}>;

export default class PinCode extends Component<Props, State> {
  static defaultProps = {
    length: 4,
  };

  onChange = (inputValue?: number, key: number) => {
    const { value, onChange } = this.props;

    const newValue = value.toString().split('');
    newValue[key] = inputValue ? inputValue.toString() : null;
    onChange(parseInt(newValue.join('')));
  };

  render() {
    const { autoFocus, label, length, value, ...restProps } = this.props;

    return (
      <div className={styles.component} role="button">
        <label className="SimpleFormField_label">{label}</label>
        {map(Array(length).fill(), (action, key) => {
          const inputValue = value ? value.toString().split('') : undefined;
          return (
            <NumericInput
              key={key}
              themeId={IDENTIFIERS.INPUT}
              skin={InputSkin}
              {...restProps}
              onChange={(value) => this.onChange(value, key)}
              value={inputValue ? inputValue[key] : undefined}
              autoFocus={autoFocus && key === 0}
              onKeyPress={this.onKeyPress}
              disabled={
                key !== 0 &&
                (!inputValue || !inputValue.hasOwnProperty(key - 1))
              }
            />
          );
        })}
      </div>
    );
  }
}
