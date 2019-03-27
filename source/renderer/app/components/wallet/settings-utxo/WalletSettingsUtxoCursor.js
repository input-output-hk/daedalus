// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletSettingsUtxoCursor.scss';

type Props = {
  x: number,
  y: number,
};

@observer
export default class WalletSettingsUtxoCursor extends Component<Props> {
  render() {
    const { x, y } = this.props;
    return (
      <g transform={`translate(${x},${y})`} className={styles.component}>
        <path />
      </g>
    );
  }
}
