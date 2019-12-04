// @flow
import BigNumber from 'bignumber.js';
import {
  DECIMAL_PLACES_IN_ADA,
  LOVELACES_PER_ADA,
} from '../config/numbersConfig';

// Symbol	  Name	              Scientific Notation
// K	      Thousand	          1.00E+03
// M	      Million	            1.00E+06
// B	      Billion	            1.00E+09
// T	      Trillion	          1.00E+12
// Q	      Quadrillion	        1.00E+15
export const formattedWalletAmount = (
  amount: BigNumber,
  withCurrency: boolean = true,
  long: boolean = true
) => {
  let formattedAmount = '';
  if (long) {
    formattedAmount = amount.toFormat(DECIMAL_PLACES_IN_ADA);
  } else if (amount.isZero()) {
    formattedAmount = '0';
  } else if (amount.lessThan(1000)) {
    formattedAmount = `${amount.round(
      DECIMAL_PLACES_IN_ADA,
      BigNumber.ROUND_DOWN
    )}`;
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
  } else {
    formattedAmount = `${amount
      .dividedBy(1000000000000000)
      .round(1, BigNumber.ROUND_DOWN)}Q`;
  }

  const { decimalSeparator } = BigNumber.config().FORMAT;
  if (!long && decimalSeparator !== '.') {
    // Only BigNumber.toFormat() method is applying correct separators.
    // Since this method is not used for condensed format (long = false)
    // the correct number format has to be applied manually.
    formattedAmount = formattedAmount.split('.').join(decimalSeparator);
  }

  if (withCurrency) formattedAmount += ' ADA';
  return formattedAmount.toString();
};

export const formattedAmountToNaturalUnits = (amount: string): string => {
  const cleanedAmount = amount
    .replace(/\./g, '') // removes all the dot separators
    .replace(/,/g, '') // removes all the comma separators
    .replace(/\s/g, '') // removes all the space separators
    .replace(/^0+/, '');
  return cleanedAmount === '' ? '0' : cleanedAmount;
};

export const formattedAmountToBigNumber = (amount: string) => {
  const cleanedAmount = amount.replace(/,/g, '');
  return new BigNumber(cleanedAmount !== '' ? cleanedAmount : 0);
};

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
