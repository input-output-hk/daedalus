import BigNumber from 'bignumber.js';
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clock-c... Remove this comment to see the full error message
import clockIcon from '../../../assets/images/clock-corner.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/no-data... Remove this comment to see the full error message
import noDataDashBigImage from '../../../assets/images/no-data-dash-big.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ThumbPoolContent.scss' or it... Remove this comment to see the full error message
import styles from './ThumbPoolContent.scss';
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import StakePool from '../../../domains/StakePool';
import {
  IS_RANKING_DATA_AVAILABLE,
  IS_SATURATION_DATA_AVAILABLE,
} from '../../../config/stakingConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/ada-sym... Remove this comment to see the full error message
import adaIcon from '../../../assets/images/ada-symbol.inline.svg';
import { formattedWalletAmount } from '../../../utils/formatters';

type Props = {
  stakePool: StakePool;
  numberOfRankedStakePools: number;
  isGridRewardsView?: boolean;
};

@observer
class ThumbPoolContent extends Component<Props> {
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
    const {
      stakePool,
      numberOfRankedStakePools,
      isGridRewardsView,
    } = this.props;
    const {
      ranking,
      nonMyopicMemberRewards,
      ticker,
      retiring,
      saturation,
      potentialRewards,
    } = stakePool;
    const color = getColorFromRange(ranking, numberOfRankedStakePools);
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
        {isGridRewardsView &&
          (IS_RANKING_DATA_AVAILABLE && nonMyopicMemberRewards ? (
            <div className={styles.rewards}>
              {this.formattedRewards(potentialRewards)}
              <SVGInline svg={adaIcon} className={styles.adaIcon} />
            </div>
          ) : (
            <div className={styles.noDataDash}>
              <SVGInline svg={noDataDashBigImage} />
            </div>
          ))}
        {!isGridRewardsView &&
          (IS_RANKING_DATA_AVAILABLE ? (
            <div
              className={styles.ranking}
              style={{
                color,
              }}
            >
              {nonMyopicMemberRewards ? (
                ranking
              ) : (
                <>
                  {numberOfRankedStakePools + 1}
                  <sup>*</sup>
                </>
              )}
            </div>
          ) : (
            <div className={styles.noDataDash}>
              <SVGInline svg={noDataDashBigImage} />
            </div>
          ))}
        {IS_SATURATION_DATA_AVAILABLE && (
          <div className={saturationClassnames}>
            <span
              style={{
                // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
                width: `${parseFloat(saturation).toFixed(2)}%`,
              }}
            />
          </div>
        )}
        {IS_RANKING_DATA_AVAILABLE ? (
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

export default ThumbPoolContent;
