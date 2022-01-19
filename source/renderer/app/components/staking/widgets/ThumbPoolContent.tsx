import BigNumber from 'bignumber.js';
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Box, Center } from '@chakra-ui/react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clock-c... Remove this comment to see the full error message
import clockIcon from '../../../assets/images/clock-corner.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/no-data... Remove this comment to see the full error message
import noDataDashBigImage from '../../../assets/images/no-data-dash-big.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ThumbPoolContent.scss' or it... Remove this comment to see the full error message
import { getColorFromRange } from '../../../utils/colors';
import StakePool from '../../../domains/StakePool';
import {
  IS_RANKING_DATA_AVAILABLE,
  IS_SATURATION_DATA_AVAILABLE,
} from '../../../config/stakingConfig';
import { formattedWalletAmount } from '../../../utils/formatters';
import {
  Container,
  Ticker,
  Rewards,
  NoDataDash,
  ClockIcon,
  NoDataDashIcon,
  Ranking,
  RankingStar,
  SaturationBar,
  Clock,
  GreyColorBand,
  ColorBand,
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

    return (
      <Container isSaturationDataAvailable={IS_SATURATION_DATA_AVAILABLE}>
        <Box h="full">
          <Ticker isSaturationDataAvailable={IS_SATURATION_DATA_AVAILABLE}>
            {ticker}
          </Ticker>
          {isGridRewardsView &&
            (IS_RANKING_DATA_AVAILABLE && nonMyopicMemberRewards ? (
              <Rewards>{this.formattedRewards(potentialRewards)}</Rewards>
            ) : (
              <NoDataDash />
            ))}
          {!isGridRewardsView &&
            (IS_RANKING_DATA_AVAILABLE ? (
              <Ranking color={color}>
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
              <Center flex="1 1 auto">
                <NoDataDashIcon svg={noDataDashBigImage} />
              </Center>
            ))}
          {IS_SATURATION_DATA_AVAILABLE && (
            <SaturationBar
              width={parseFloat(saturation).toFixed(2)}
              color={color}
            />
          )}
        </Box>
        <Box alignSelf="flex-end" w="full">
          {IS_RANKING_DATA_AVAILABLE ? (
            <>
              {retiring && (
                <Clock>
                  <ClockIcon svg={clockIcon} />
                </Clock>
              )}
              <ColorBand color={color} />
            </>
          ) : (
            <GreyColorBand />
          )}
        </Box>
      </Container>
    );
  }
}

export default ThumbPoolContent;
