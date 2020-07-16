// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import clockIcon from '../../../assets/images/clock-corner.inline.svg';
import styles from './ThumbPoolContent.scss';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import StakePool from '../../../domains/StakePool';

type Props = {
  stakePool: StakePool,
  numberOfStakePools: number,
};

@observer
export default class ThumbPoolContent extends Component<Props> {
  render() {
    const { stakePool, numberOfStakePools } = this.props;

    const { ranking, ticker, retiring, saturation } = stakePool;
    const color = getColorFromRange(ranking, numberOfStakePools);

    const saturationClassnames = classnames([
      styles.saturationBar,
      styles[getSaturationColor(saturation)],
    ]);

    return (
      <div className={styles.component}>
        <div className={styles.ticker}>{ticker}</div>
        <div className={styles.ranking} style={{ color }}>
          {ranking}
        </div>
        <div className={saturationClassnames}>
          <span
            style={{
              width: `${parseFloat(saturation.toFixed(2))}%`,
            }}
          />
        </div>

        {retiring && (
          <div className={styles.clock}>
            <SVGInline svg={clockIcon} className={styles.clockIcon} />
          </div>
        )}
        <div
          className={styles.colorBand}
          style={{
            background: color,
          }}
        />
      </div>
    );
  }
}
