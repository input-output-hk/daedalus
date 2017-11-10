// @flow
import BigNumber from 'bignumber.js';
import { DECIMAL_PLACES_IN_ADA } from '../../config/numbersConfig';

export const formattedWalletAmount = (
  amount: BigNumber,
  withCurrency: boolean = true,
) => {
  let formattedAmount = amount.toFormat(DECIMAL_PLACES_IN_ADA);

  if (withCurrency) formattedAmount += ' ADA';

  return formattedAmount.toString();
};
