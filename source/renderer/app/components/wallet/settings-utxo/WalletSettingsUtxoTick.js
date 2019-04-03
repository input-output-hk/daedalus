// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './WalletSettingsUtxoTick.scss';

export type TickProps = {
  x: number,
  y: number,
  payload: {
    value: number,
  },
  textAnchor: 'start' | 'end',
  getUtxoWalletPrettyAmount?: Function,
  vertical?: boolean,
};

@observer
export default class WalletSettingsUtxoTick extends Component<TickProps> {
  render() {
    const {
      x,
      y,
      payload: { value },
      vertical,
      getUtxoWalletPrettyAmount,
      textAnchor,
    } = this.props;
    const componentStyles = classnames([
      styles.component,
      vertical ? styles.vertical : null,
    ]);
    return (
      <g transform={`translate(${x},${y})`} className={componentStyles}>
        <text textAnchor={textAnchor}>
          {getUtxoWalletPrettyAmount ? getUtxoWalletPrettyAmount(value) : value}
        </text>
      </g>
    );
  }
}
