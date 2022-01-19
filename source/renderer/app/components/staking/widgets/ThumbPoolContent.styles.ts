// @flow
import SVGInline from 'react-svg-inline';
import styled from '@emotion/styled';

export const AdaIcon = styled(SVGInline)`
  svg {
    height: 11px;
    width: 10px;

    & > g {
      & > g {
        stroke: var(--theme-staking-wallet-row-ticker-ada-icon-fill-color);
      }
    }
  }
`;

export const ClockIcon = styled(SVGInline)`
  svg {
    height: 15px;
    width: 15px;
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
