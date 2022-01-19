import React from 'react';
import SVGInline from 'react-svg-inline';
import { Box, Text, Center, Flex } from '@chakra-ui/react';
import styled from '@emotion/styled';
import clockIcon from '../../../assets/images/clock-corner.inline.svg';
import adaIcon from '../../../assets/images/ada-symbol.inline.svg';
import noDataDashBigImage from '../../../assets/images/no-data-dash-big.inline.svg';

export const Container = ({ children, isSaturationDataAvailable }) => (
  <Flex
    h="16"
    w="20"
    flexDirection="column"
    pt={!isSaturationDataAvailable ? '3' : '2'}
    pos="relative"
  >
    {children}
  </Flex>
);

export const Ticker = ({ children, isSaturationDataAvailable }) => (
  <Center mb={!isSaturationDataAvailable ? '1' : 'px'}>
    <Text
      fontWeight="semibold"
      fontSize="sm"
      sx={{ color: 'var(--theme-staking-stake-pool-ticker-color)' }}
      lineHeight="none"
    >
      {children}
    </Text>
  </Center>
);

export const Rewards = ({ children }) => (
  <Center py="0.5" pos="relative" flex="1 1 auto">
    <Text fontSize="sm" fontWeight="semibold">
      {children}
    </Text>
    <AdaIcon svg={adaIcon} />
  </Center>
);

export const NoDataDash = ({ children }) => (
  <Center flex="1 1 auto">
    <NoDataDashIcon svg={noDataDashBigImage} />
  </Center>
);

export const Ranking = ({ children, color }) => (
  <Center flex="1" style={{ color }} mt="1">
    <Text fontSize="xl" fontWeight="bold" lineHeight="none">
      {children}
    </Text>
  </Center>
);

export const RankingStar = ({ children }) => (
  <Text display="inline-block">{children}</Text>
);

export const AdaIcon = styled(SVGInline)`
  svg {
    height: 11px;
    margin-left: 3px;
    width: 10px;

    & > g {
      & > g {
        stroke: var(--theme-staking-wallet-row-ticker-ada-icon-fill-color);
      }
    }
  }
`;

export const NoDataDashIcon = styled(SVGInline)`
  svg {
    height: 3px;
    width: 12px;

    path {
      fill: var(--theme-staking-stake-pool-grey-color) !important;
      opacity: 1 !important;
    }
  }
`;

export const Clock = ({ children }) => (
  <Box pos="absolute" right="0" top="0">
    {children}
  </Box>
);

export const ClockIcon = styled(SVGInline)`
  svg {
    height: 15px;
    width: 15px;
  }
`;

export const ColorBand = ({ color }) => (
  <Box h="1" w="full" sx={{ background: color }} flexShrink="0" />
);

export const GreyColorBand = () => (
  <Box
    h="1"
    w="full"
    sx={{ background: 'var(--theme-staking-stake-pool-grey-color)' }}
    flexShrink="0"
  />
);

export const SaturationBar = ({ width, color }) => (
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
          width: `${width}%`,
        }}
        bg={`stakePoolSaturation.${color}`}
      />
    </Flex>
  </Center>
);
