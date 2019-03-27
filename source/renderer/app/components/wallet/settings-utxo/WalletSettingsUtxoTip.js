// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './WalletSettingsUtxoTip.scss';

type Props = {
  x: number,
  y: number,
  payload: {
    value: number,
  },
  textAnchor: 'start' | 'end',
  getPrettyAmount?: Function,
  vertical: boolean,
};

@observer
export default class WalletSettingsUtxoTip extends Component<Props> {
  render() {
    const {
      x,
      y,
      payload: { value },
      vertical,
      getPrettyAmount,
      textAnchor,
    } = this.props;
    const componentStyles = classnames([
      styles.component,
      vertical ? styles.vertical : null,
    ]);
    return (
      <g transform={`translate(${x},${y})`} className={componentStyles}>
        <text textAnchor={textAnchor}>
          {getPrettyAmount ? getPrettyAmount(value) : value}
        </text>
      </g>
    );
  }
}
