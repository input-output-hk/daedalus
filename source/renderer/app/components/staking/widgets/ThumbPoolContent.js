// @flow
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import clockIcon from '../../../assets/images/clock-corner.inline.svg';
import noDataDashBigImage from '../../../assets/images/no-data-dash-big.inline.svg';
import styles from './ThumbPoolContent.scss';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import StakePool from '../../../domains/StakePool';
import {
  IS_RANKING_DATA_AVAILABLE,
  IS_SATURATION_DATA_AVAILABLE,
} from '../../../config/stakingConfig';

type Props = {
  stakePool: StakePool,
  numberOfStakePools: number,
};

@observer
export default class ThumbPoolContent extends Component<Props> {
  render() {
    const { stakePool, numberOfStakePools } = this.props;
    const {
      ranking,
      nonMyopicMemberRewards,
      ticker,
      retiring,
      saturation,
    } = stakePool;
    const color = getColorFromRange(ranking, numberOfStakePools);

    const componentClassnames = classnames([
      styles.component,
      !IS_SATURATION_DATA_AVAILABLE ? styles.hideSaturation : null,
    ]);

    const saturationClassnames = classnames([
      styles.saturationBar,
      styles[getSaturationColor(saturation)],
    ]);

    return (
      <div className={componentClassnames}>
        <div className={styles.ticker}>{ticker}</div>
        {IS_RANKING_DATA_AVAILABLE && nonMyopicMemberRewards ? (
          <div className={styles.ranking} style={{ color }}>
            {ranking}
          </div>
        ) : (
          <div className={styles.noDataDash}>
            <SVGInline svg={noDataDashBigImage} />
          </div>
        )}
        {IS_SATURATION_DATA_AVAILABLE && (
          <div className={saturationClassnames}>
            <span
              style={{
                width: `${parseFloat(saturation.toFixed(2))}%`,
              }}
            />
          </div>
        )}
        {IS_RANKING_DATA_AVAILABLE && nonMyopicMemberRewards ? (
          <>
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
          </>
        ) : (
          <div className={styles.greyColorBand} />
        )}
      </div>
    );
  }
}
