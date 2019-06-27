// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import classnames from 'classnames';
import styles from './DonutRing.scss';

export const DONUT_RING_SIZES = {
  SMALL: 'small',
  MEDIUM: 'medium',
  LARGE: 'large',
};

type Props = { percentage: number, size: string };

@observer
export default class DonutRing extends Component<Props> {
  static defaultProps = {
    percentage: 0,
    size: DONUT_RING_SIZES.MEDIUM,
  };

  render() {
    const { percentage, size } = this.props;
    const deg = (1 - percentage / 100) * 360;
    const percentageClassName =
      percentage < 50 ? 'lessThanHalf' : 'moreThanHalf';
    const componentClassNames = classnames([
      styles.component,
      styles[size],
      styles[percentageClassName],
    ]);
    let deg1 = `${deg + 90}deg`;
    let deg2 = '0deg';

    if (percentage < 50) {
      deg1 = '90deg';
      deg2 = `${deg}deg`;
    }

    return (
      <div className={componentClassNames}>
        <div
          className={classnames([styles.slice, styles.one])}
          style={{ transform: `rotate(${deg1})` }}
        />
        <div
          className={classnames([styles.slice, styles.two])}
          style={{ transform: `rotate(${deg2})` }}
        />
        <div className={styles.center} />
      </div>
    );
  }
}
