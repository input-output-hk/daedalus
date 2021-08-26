// @flow
import BigNumber from 'bignumber.js';
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import { defineMessages, intlShape } from 'react-intl';
import { PopOver } from 'react-polymorph/lib/components/PopOver';
import classnames from 'classnames';
import clockIcon from '../../../assets/images/clock-corner.inline.svg';
import crossIcon from '../../../assets/images/cross-corner.inline.svg';
import noDataDashBigImage from '../../../assets/images/no-data-dash-big.inline.svg';
import styles from './ThumbPoolContent.scss';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import StakePool from '../../../domains/StakePool';
import {
  IS_RANKING_DATA_AVAILABLE,
  IS_SATURATION_DATA_AVAILABLE,
} from '../../../config/stakingConfig';
import adaIcon from '../../../assets/images/ada-symbol.inline.svg';
import { formattedWalletAmount } from '../../../utils/formatters';

type Props = {
  stakePool: StakePool,
  numberOfRankedStakePools: number,
  isGridRewardsView?: boolean,
};

const messages = defineMessages({
  pledgeNotMetPopOver: {
    id: 'staking.stakePools.tooltip.pledgeNotMet.popover',
    defaultMessage:
      '!!!This pool has not met its pledge requirements. This means that the pool will not produce blocks or generate rewards until the pledge is met.',
    description: '"pledgeNotMet" popover for the Stake Pools Tooltip page.',
  },
});

@observer
export default class ThumbPoolContent extends Component<Props> {
  static contextTypes = {
    intl: intlShape.isRequired,
  };

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
    const { intl } = this.context;
    const {
      stakePool,
      numberOfRankedStakePools,
      isGridRewardsView,
    } = this.props;
    const {
      ranking,
      ticker,
      retiring,
      pledgeNotMet,
      saturation,
      potentialRewards,
    } = stakePool;
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
        {isGridRewardsView &&
          (IS_RANKING_DATA_AVAILABLE && !potentialRewards.isZero() ? (
            <div className={styles.rewards}>
              {this.formattedRewards(potentialRewards)}
              <SVGInline svg={adaIcon} className={styles.adaIcon} />
            </div>
          ) : (
            <div className={styles.rewards}>?</div>
          ))}
        {!isGridRewardsView &&
          (IS_RANKING_DATA_AVAILABLE && !pledgeNotMet ? (
            <div className={styles.ranking} style={{ color }}>
              {ranking}
            </div>
          ) : (
            <div className={styles.noDataDash}>
              <SVGInline svg={noDataDashBigImage} />
            </div>
          ))}
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
            {pledgeNotMet && (
              <div className={styles.cornerIcon}>
                <PopOver
                  content={intl.formatMessage(messages.pledgeNotMetPopOver)}
                  zIndex={10000}
                >
                  <SVGInline svg={crossIcon} />
                </PopOver>
              </div>
            )}
            {!pledgeNotMet && retiring && (
              <div className={styles.cornerIcon}>
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
