import React, { Component } from 'react';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './WalletUtxoCursor.scss' or it... Remove this comment to see the full error message
import styles from './WalletUtxoCursor.scss';

type CursorProps = {
  height?: number;
  offsetWidth?: number;
  x?: number;
  width?: number;
};
const OFFSET_TOP = 20;
const OFFSET_BOTTOM = 60;

@observer
class WalletUtxoCursor extends Component<CursorProps> {
  render() {
    let { x, width, height } = this.props;
    const { offsetWidth } = this.props;
    // Avoid flow errors for props from HOC
    x = x || 0;
    width = width || 0;
    height = height || 0;
    const verticalPosition = height + OFFSET_TOP + OFFSET_BOTTOM;
    const calculatedWidth = offsetWidth ? (width - offsetWidth) / 2 : 0;
    return (
      <g
        transform={`translate(${x + calculatedWidth},${-OFFSET_TOP})`}
        className={styles.component}
      >
        <path
          d={`M 0.0,0 h ${offsetWidth || 0} v ${verticalPosition} h -${
            offsetWidth || 0
          } Z`}
        />
      </g>
    );
  }
}

export default WalletUtxoCursor;
