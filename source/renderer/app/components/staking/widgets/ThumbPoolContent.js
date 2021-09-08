// @flow
import BigNumber from 'bignumber.js';
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import clockIcon from '../../../assets/images/clock-icon.inline.svg';
import noDataDashBigImage from '../../../assets/images/no-data-dash-big.inline.svg';
import styles from './ThumbPoolContent.scss';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import StakePool from '../../../domains/StakePool';
import {
  IS_RANKING_DATA_AVAILABLE,
  IS_SATURATION_DATA_AVAILABLE,
} from '../../../config/stakingConfig';
import { formattedWalletAmount } from '../../../utils/formatters';

type Props = {
  stakePool: StakePool,
  numberOfRankedStakePools: number,
};

@observer
export default class ThumbPoolContent extends Component<Props> {
  formattedRewards = (potentialRewards: BigNumber) => {
    const potentialRewardsAsString = formattedWalletAmount(potentialRewards);
    let targetLength = 4;
    if (potentialRewardsAsString.includes('.')) {
      targetLength++;
    }
    if (potentialRewardsAsString.includes(',')) {
      targetLength++;
    }
    if (potentialRewardsAsString.includes(' ')) {
      targetLength++;
    }
    return potentialRewardsAsString.substring(0, targetLength);
  };

  render() {
    const { stakePool, numberOfRankedStakePools } = this.props;
    const { ranking, ticker, retiring, pledgeNotMet, saturation } = stakePool;
    const color =
      !pledgeNotMet && getColorFromRange(ranking, numberOfRankedStakePools);

    const componentClassnames = classnames([
      styles.component,
      !IS_SATURATION_DATA_AVAILABLE ? styles.hideSaturation : null,
    ]);

    const saturationClassnames = classnames([
      styles.saturationBar,
      styles[getSaturationColor(saturation)],
    ]);

    const colorBandClassnames = classnames([
      styles.colorBand,
      pledgeNotMet ? styles.pledgeNotMet : null,
    ]);

    return (
      <div className={componentClassnames}>
        <div className={styles.ticker}>{ticker}</div>
        {IS_RANKING_DATA_AVAILABLE && !pledgeNotMet ? (
          <div className={styles.ranking} style={{ color }}>
            {ranking}
          </div>
        ) : (
          <div className={styles.noDataDash}>
            <SVGInline svg={noDataDashBigImage} />
          </div>
        )}
        {IS_SATURATION_DATA_AVAILABLE && !pledgeNotMet && (
          <div className={saturationClassnames}>
            <span
              style={{
                width: `${parseFloat(saturation).toFixed(2)}%`,
              }}
            />
          </div>
        )}
        {IS_RANKING_DATA_AVAILABLE ? (
          <>
            {retiring && (
              <div className={styles.retiringIcon}>
                <SVGInline svg={clockIcon} />
              </div>
            )}

            <div
              className={colorBandClassnames}
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
