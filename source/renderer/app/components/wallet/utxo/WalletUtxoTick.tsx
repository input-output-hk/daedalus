import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletUtxoTick.scss' or its ... Remove this comment to see the full error message
import styles from './WalletUtxoTick.scss';

export type TickProps = {
  x: number;
  y: number;
  payload: {
    value: number;
  };
  textAnchor: 'start' | 'end';
  vertical?: boolean;
};

@observer
class WalletUtxoTick extends Component<TickProps> {
  render() {
    const {
      x,
      y,
      payload: { value },
      vertical,
      textAnchor,
    } = this.props;
    const componentStyles = classnames([
      styles.component,
      vertical ? styles.vertical : null,
    ]);
    return (
      <g transform={`translate(${x},${y})`} className={componentStyles}>
        <text textAnchor={textAnchor}>{value}</text>
      </g>
    );
  }
}

export default WalletUtxoTick;
