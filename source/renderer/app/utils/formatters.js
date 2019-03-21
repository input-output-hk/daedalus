// @flow
import BigNumber from 'bignumber.js';
import {
  DECIMAL_PLACES_IN_ADA,
  LOVELACES_PER_ADA,
} from '../config/numbersConfig';

export const formattedWalletAmount = (
  amount: BigNumber,
  withCurrency: boolean = true
) => {
  let formattedAmount = amount.toFormat(DECIMAL_PLACES_IN_ADA);

  if (withCurrency) formattedAmount += ' ADA';

  return formattedAmount.toString();
};

export const formattedAmountToBigNumber = (amount: string) => {
  const cleanedAmount = amount.replace(/,/g, '');
  return new BigNumber(cleanedAmount !== '' ? cleanedAmount : 0);
};

export const formattedAmountToNaturalUnits = (amount: string): string => {
  const cleanedAmount = amount
    .replace('.', '')
    .replace(/,/g, '')
    .replace(/^0+/, '');
  return cleanedAmount === '' ? '0' : cleanedAmount;
};

export const formattedAmountWithoutTrailingZeros = (amount: string): string =>
  amount.replace(/0+$/, '').replace(/\.$/, '');

export const formattedAmountToLovelace = (amount: string): number =>
  parseInt(formattedAmountToBigNumber(amount).times(LOVELACES_PER_ADA), 10);

export const formattedPrettyAmount = (amount: BigNumber) => {
  let ada = amount / LOVELACES_PER_ADA;
  if (ada === 1000) ada = '1K';
  if (ada === 10000) ada = '10K';
  if (ada === 100000) ada = '100K';
  if (ada === 1000000) ada = '1M';
  if (ada === 10000000) ada = '10M';
  if (ada === 100000000) ada = '100M';
  if (ada === 1000000000) ada = '1B';
  if (ada === 10000000000) ada = '10B';
  if (ada === 45000000000) ada = '45B';
  return ada;
};
