import SVGInline from 'react-svg-inline';
import styled from '@emotion/styled';

export const Container = styled.div`
  height: 69px;
  position: relative;
  width: 78px;

  padding-top: ${({ isSaturationDataAvailable }) =>
    isSaturationDataAvailable ? '9px' : '11px'};
`;

export const Ticker = styled.div`
  color: var(--theme-staking-stake-pool-ticker-color);
  font-family: var(--font-semibold);
  font-size: 14px;
  letter-spacing: -0.5px;
  line-height: 1;
  text-align: center;
  margin-bottom: ${({ isSaturationDataAvailable }) =>
    isSaturationDataAvailable ? '1px' : '5px'};
`;

export const Ranking = styled.div`
  font-family: var(--font-bold);
  font-size: 20px;
  font-weight: bold;
  position: relative;
  text-align: center;
`;

export const RankingStar = styled.sub`
  font-family: Verdana;
  font-size: 14px;
  margin-left: -1px;
  position: absolute;
  top: -2px;
`;

export const Rewards = styled.div`
  font-family: var(--font-regular);
  font-size: 14px;
  font-weight: 600;
  letter-spacing: -0.5px;
  line-height: 1.57;
  padding: 3px 0;
  position: relative;
  text-align: center;
`;

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

export const NoDataDash = styled.div`
  align-items: center;
  display: flex;
  height: 27px;
  justify-content: center;
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

export const Clock = styled.div`
  background: var(--theme-staking-stake-pool-retirement-background-color);
  border-radius: 0 2px 0 4px;
  height: 18px;
  position: absolute;
  right: 0;
  top: 0;
  width: 18px;
`;

export const ClockIcon = styled(SVGInline)`
  svg {
    height: 15px;
    width: 15px;
  }
`;

export const ColorBand = styled.div`
  bottom: 0;
  display: block;
  height: 5px;
  left: 0;
  position: absolute;
  width: 100%;
`;

export const GreyColorBand = styled.div`
  bottom: 0;
  display: block;
  height: 5px;
  left: 0;
  position: absolute;
  width: 100%;
`;

const colors = {
  green: 'var(--theme-staking-stake-pool-saturation-green-color)',
  orange: 'var(--theme-staking-stake-pool-saturation-orange-color)',
  red: 'var(--theme-staking-stake-pool-saturation-red-color)',
  yellow: 'var(--theme-staking-stake-pool-saturation-yellow-color)',
};

export const SaturationBar = styled.div`
  bottom: 0;
  display: block;
  height: 5px;
  left: 0;
  position: absolute;
  width: 100%;

  span {
    border-radius: 2px;
    bottom: 0;
    height: inherit;
    left: 0;
    position: absolute;
    top: 0;
  }

  background: ${({ color }) => colors[color]};
`;
