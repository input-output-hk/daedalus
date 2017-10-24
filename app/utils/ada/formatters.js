// @flow
import BigNumber from 'bignumber.js';
import { DECIMAL_PLACES_IN_ADA } from '../../config/numbersConfig';

export const formattedWalletAmount = (amount: BigNumber) => (
  `${amount.toFormat(DECIMAL_PLACES_IN_ADA)} ADA`
);
