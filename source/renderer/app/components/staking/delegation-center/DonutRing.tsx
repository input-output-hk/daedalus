import React, { Component } from 'react';
import { observer } from 'mobx-react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './DonutRing.scss' or its corre... Remove this comment to see the full error message
import styles from './DonutRing.scss';

type Props = {
  percentage: number;
  sqSize: number;
  strokeWidth: number;
  showText?: boolean;
};

@observer
class DonutRing extends Component<Props> {
  static defaultProps = {
    showText: false,
  };

  render() {
    const { percentage, sqSize, strokeWidth, showText } = this.props;
    const invertedPercentage = 100 - percentage;
    const radius = (sqSize - strokeWidth) / 2;
    const viewBox = `0 0 ${sqSize} ${sqSize}`;
    const dashArray = radius * Math.PI * 2;
    const dashOffset = dashArray - (dashArray * invertedPercentage) / 100;
    const rotateDeg = -((invertedPercentage / 100) * 360 + 90);
    return (
      <div className={styles.component}>
        <svg width={sqSize} height={sqSize} viewBox={viewBox}>
          <circle
            className={styles.circleProgress}
            cx={sqSize / 2}
            cy={sqSize / 2}
            r={radius}
            strokeWidth={`${strokeWidth}px`}
          />
          <circle
            className={styles.circleBackground}
            cx={sqSize / 2}
            cy={sqSize / 2}
            r={radius}
            strokeWidth={`${strokeWidth}px`}
            transform={`rotate(${rotateDeg} ${sqSize / 2} ${sqSize / 2})`}
            style={{
              strokeDasharray: dashArray,
              strokeDashoffset: dashOffset,
            }}
          />
          {showText && (
            <text
              className={styles.circleText}
              x="50%"
              y="50%"
              dy=".3em"
              textAnchor="middle"
            >
              {`${invertedPercentage}%`}
            </text>
          )}
        </svg>
      </div>
    );
  }
}

export default DonutRing;
