// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletSettingsUtxoCursor.scss';

export type CursorProps = {
  x: number,
  width: number,
  height: number,
};

const OFFSET_TOP = 20;
const OFFSET_BOTTOM = 60;

@observer
export default class WalletSettingsUtxoCursor extends Component<CursorProps> {
  static defaultProps = {
    x: 0,
    width: 0,
    height: 0,
  };

  render() {
    const { x, width, height } = this.props;
    const verticalPosition = height + OFFSET_TOP + OFFSET_BOTTOM;

    return (
      <g
        transform={`translate(${x},${-OFFSET_TOP})`}
        className={styles.component}
      >
        <path d={`M 0.0,0 h ${width} v ${verticalPosition} h -${width} Z`} />
      </g>
    );
  }
}
