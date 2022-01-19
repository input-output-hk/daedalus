import BigNumber from 'bignumber.js';
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import { Box, Text, Center, Flex } from '@chakra-ui/react';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/clock-c... Remove this comment to see the full error message
import clockIcon from '../../../assets/images/clock-corner.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/no-data... Remove this comment to see the full error message
import noDataDashBigImage from '../../../assets/images/no-data-dash-big.inline.svg';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module './ThumbPoolContent.scss' or it... Remove this comment to see the full error message
import { getColorFromRange, getSaturationColor } from '../../../utils/colors';
import StakePool from '../../../domains/StakePool';
import {
  IS_RANKING_DATA_AVAILABLE,
  IS_SATURATION_DATA_AVAILABLE,
} from '../../../config/stakingConfig';
// @ts-ignore ts-migrate(2307) FIXME: Cannot find module '../../../assets/images/ada-sym... Remove this comment to see the full error message
import adaIcon from '../../../assets/images/ada-symbol.inline.svg';
import { formattedWalletAmount } from '../../../utils/formatters';
import { AdaIcon, ClockIcon, NoDataDashIcon } from './ThumbPoolContent.styles';

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
      <Flex
        h="16"
        w="20"
        flexDirection="column"
        pt={!IS_SATURATION_DATA_AVAILABLE ? '3' : '2'}
        pos="relative"
      >
        <Box h="full">
          <Center mb={!IS_SATURATION_DATA_AVAILABLE ? '1' : 'px'}>
            <Text
              fontWeight="semibold"
              fontSize="sm"
              sx={{ color: 'var(--theme-staking-stake-pool-ticker-color)' }}
              lineHeight="none"
            >
              {ticker}
            </Text>
          </Center>
          {isGridRewardsView &&
            (IS_RANKING_DATA_AVAILABLE && nonMyopicMemberRewards ? (
              <Center py="0.5" pos="relative" flex="1 1 auto">
                <Text fontSize="sm" fontWeight="semibold">
                  {this.formattedRewards(potentialRewards)}
                </Text>
                <AdaIcon svg={adaIcon} />
              </Center>
            ) : (
              <Center flex="1 1 auto">
                <NoDataDashIcon svg={noDataDashBigImage} />
              </Center>
            ))}
          {!isGridRewardsView &&
            (IS_RANKING_DATA_AVAILABLE ? (
              <Center flex="1" style={{ color }} mt="1">
                <Text fontSize="xl" fontWeight="bold" lineHeight="none">
                  {nonMyopicMemberRewards ? (
                    ranking
                  ) : (
                    <>
                      {numberOfRankedStakePools + 1}
                      <Text display="inline-block">*</Text>
                    </>
                  )}
                </Text>
              </Center>
            ) : (
              <Center flex="1 1 auto">
                <NoDataDashIcon svg={noDataDashBigImage} />
              </Center>
            ))}
          {IS_SATURATION_DATA_AVAILABLE && (
            <Center my="1">
              <Flex
                h="1"
                w="10"
                sx={{
                  background:
                    'var(--theme-staking-stake-pool-saturation-background-color)',
                }}
                borderRadius="sm"
              >
                <Box
                  as="span"
                  h="1"
                  sx={{
                    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
                    width: `${parseFloat(saturation).toFixed(2)}%`,
                  }}
                  bg={`stakePoolSaturation.${getSaturationColor(saturation)}`}
                />
              </Flex>
            </Center>
          )}
        </Box>
        <Box alignSelf="flex-end" w="full">
          {IS_RANKING_DATA_AVAILABLE ? (
            <>
              {retiring && (
                <Box pos="absolute" right="0" top="0">
                  <ClockIcon svg={clockIcon} />
                </Box>
              )}
              <Box h="1" w="full" sx={{ background: color }} flexShrink="0" />
            </>
          ) : (
            <Box
              h="1"
              w="full"
              sx={{ background: 'var(--theme-staking-stake-pool-grey-color)' }}
              flexShrink="0"
            />
          )}
        </Box>
      </Flex>
    );
  }
}

export default ThumbPoolContent;
