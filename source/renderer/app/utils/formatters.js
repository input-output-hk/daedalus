// @flow
import BigNumber from 'bignumber.js';
import {
  DECIMAL_PLACES_IN_ADA,
  LOVELACES_PER_ADA,
} from '../config/numbersConfig';
import { NUMBER_FORMATS } from '../../../common/types/number.types';

// Symbol	  Name	              Scientific Notation
// K	      Thousand	          1.00E+03
// M	      Million	            1.00E+06
// B	      Billion	            1.00E+09
// T	      Trillion	          1.00E+12
// Q	      Quadrillion	        1.00E+15
export const formattedWalletAmount = (
  amount: BigNumber,
  withCurrency: boolean = true,
  long: boolean = false,
  currentNumberFormat?: string
) => {
  let formattedAmount = '';
  if (long) {
    formattedAmount = amount.toFormat(DECIMAL_PLACES_IN_ADA);
  } else if (amount.isZero()) {
    formattedAmount = '0';
  } else if (amount.lessThan(1)) {
    formattedAmount = amount;
  } else if (amount.lessThan(1000)) {
    formattedAmount = amount.round(1, BigNumber.ROUND_DOWN);
  } else if (amount.lessThan(1000000)) {
    formattedAmount = `${amount
      .dividedBy(1000)
      .round(1, BigNumber.ROUND_DOWN)}K`;
  } else if (amount.lessThan(1000000000)) {
    formattedAmount = `${amount
      .dividedBy(1000000)
      .round(1, BigNumber.ROUND_DOWN)}M`;
  } else if (amount.lessThan(1000000000000)) {
    formattedAmount = `${amount
      .dividedBy(1000000000)
      .round(1, BigNumber.ROUND_DOWN)}B`;
  } else if (amount.lessThan(1000000000000000)) {
    formattedAmount = `${amount
      .dividedBy(1000000000000)
      .round(1, BigNumber.ROUND_DOWN)}T`;
  } else if (amount.lessThan(1000000000000000000)) {
    formattedAmount = `${amount
      .dividedBy(1000000000000000)
      .round(1, BigNumber.ROUND_DOWN)}Q`;
  }

  if (currentNumberFormat && NUMBER_FORMATS[currentNumberFormat]) {
    const { groupSeparator } = NUMBER_FORMATS[currentNumberFormat];
    formattedAmount = formattedAmount.split('.').join(groupSeparator);
  }
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

export const formattedLovelaceToAmount = (lovelace: number): number =>
  formattedAmountToBigNumber(String(lovelace))
    .dividedBy(LOVELACES_PER_ADA)
    .toNumber();

export const formattedBytesToSize = (bytes: number): string => {
  const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB'];
  if (bytes === 0) return 'n/a';
  const i = parseInt(
    Math.floor(Math.log(Math.abs(bytes)) / Math.log(1024)),
    10
  );
  if (i === 0) return `${bytes} ${sizes[i]})`;
  return `${(bytes / 1024 ** i).toFixed(1)} ${sizes[i]}`;
};
