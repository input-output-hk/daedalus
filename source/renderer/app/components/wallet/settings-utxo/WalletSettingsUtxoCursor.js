// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import styles from './WalletSettingsUtxoCursor.scss';

type CursorProps = {
  x?: number,
  width?: number,
  height?: number,
};

const OFFSET_TOP = 20;
const OFFSET_BOTTOM = 60;

@observer
export default class WalletSettingsUtxoCursor extends Component<CursorProps> {
  render() {
    let { x, width, height } = this.props;

    // Avoid flow errors for props from HOC
    x = x || 0;
    width = width || 0;
    height = height || 0;

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
