import BigNumber from 'bignumber.js';
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import SVGInline from 'react-svg-inline';
import classnames from 'classnames';
import clockIcon from '../../../assets/images/clock-corner.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/no-data... Remove this comment to see the full error message
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
import {
  Container,
  Ticker,
  Ranking,
  RankingStar,
  Rewards,
  AdaIcon,
  NoDataDash,
  NoDataDashIcon,
  Clock,
  ClockIcon,
  ColorBand,
  GreyColorBand,
  SaturationBar,
} from './ThumbPoolContent.styles';

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

    const saturationClassnames = classnames([
      styles.saturationBar,
      styles[getSaturationColor(saturation)],
    ]);

    return (
      <Container isSaturationDataAvailable={IS_SATURATION_DATA_AVAILABLE}>
        <Ticker isSaturationDataAvailable={IS_SATURATION_DATA_AVAILABLE}>
          {ticker}
        </Ticker>
        {isGridRewardsView &&
          (IS_RANKING_DATA_AVAILABLE && nonMyopicMemberRewards ? (
            <Rewards>
              {this.formattedRewards(potentialRewards)}
              <AdaIcon svg={adaIcon} />
            </Rewards>
          ) : (
            <div className={styles.noDataDash}>
              <SVGInline svg={noDataDashBigImage} />
            </div>
          ))}
        {!isGridRewardsView &&
          (IS_RANKING_DATA_AVAILABLE ? (
            <Ranking style={{ color }}>
              {nonMyopicMemberRewards ? (
                ranking
              ) : (
                <>
                  {numberOfRankedStakePools + 1}
                  <RankingStar>*</RankingStar>
                </>
              )}
            </Ranking>
          ) : (
            <NoDataDash>
              <NoDataDashIcon svg={noDataDashBigImage} />
            </NoDataDash>
          ))}
        {IS_SATURATION_DATA_AVAILABLE && (
          <SaturationBar>
            <span
              style={{
                // @ts-ignore Argument of type 'number' is not assignable to parameter of type 'string'.ts(2345)
                width: `${parseFloat(saturation).toFixed(2)}%`,
              }}
            />
          </SaturationBar>
        )}
        {IS_RANKING_DATA_AVAILABLE ? (
          <>
            {retiring && (
              <Clock>
                <ClockIcon svg={clockIcon} />
              </Clock>
            )}
            <ColorBand
              style={{
                background: color,
              }}
            />
          </>
        ) : (
          <GreyColorBand />
        )}
      </Container>
    );
  }
}

export default ThumbPoolContent;
