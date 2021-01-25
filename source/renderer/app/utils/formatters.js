// @flow
import BigNumber from 'bignumber.js';
import moment from 'moment';
import {
  DECIMAL_PLACES_IN_ADA,
  LOVELACES_PER_ADA,
} from '../config/numbersConfig';
import { momentLocales } from '../../../common/types/locales.types';
import type { DownloadData } from '../../../common/types/downloadManager.types';
import type { Locale } from '../../../common/types/locales.types';
import type { Currency } from '../types/currencyTypes.js';

export const formattedWalletAmount = (
  amount: BigNumber,
  withCurrency: boolean = true,
  long: boolean = true
) => {
  let formattedAmount = long
    ? amount.toFormat(DECIMAL_PLACES_IN_ADA)
    : shortNumber(amount);
  const { decimalSeparator } = BigNumber.config().FORMAT;
  if (!long && decimalSeparator !== '.') {
    // Only BigNumber.toFormat() method is applying correct separators.
    // Since this method is not used for condensed format (long = false)
    // the correct number format has to be applied manually.
    formattedAmount = formattedAmount.split('.').join(decimalSeparator);
  }
  if (withCurrency) {
    formattedAmount = `${formattedAmount} ADA`;
  }
  return formattedAmount.toString();
};

export const formattedWalletCurrencyAmount = (
  amount: BigNumber,
  currencyRate: number,
  currency: ?Currency
) =>
  `${amount.times(currencyRate).toFormat(2)} ${
    currency ? currency.symbol.toUpperCase() : ''
  }`;

// Symbol   Name                Scientific Notation
// K        Thousand            1.00E+03
// M        Million             1.00E+06
// B        Billion             1.00E+09
// T        Trillion            1.00E+12
// Q        Quadrillion         1.00E+15
export const shortNumber = (value: number | BigNumber): string => {
  const amount = new BigNumber(value);
  let formattedAmount = '';
  if (amount.isZero()) {
    formattedAmount = '0';
  } else if (amount.isLessThan(1000)) {
    formattedAmount = `${amount.decimalPlaces(
      DECIMAL_PLACES_IN_ADA,
      BigNumber.ROUND_DOWN
    )}`;
  } else if (amount.isLessThan(1000000)) {
    formattedAmount = `${amount
      .dividedBy(1000)
      .decimalPlaces(1, BigNumber.ROUND_DOWN)}K`;
  } else if (amount.isLessThan(1000000000)) {
    formattedAmount = `${amount
      .dividedBy(1000000)
      .decimalPlaces(1, BigNumber.ROUND_DOWN)}M`;
  } else if (amount.isLessThan(1000000000000)) {
    formattedAmount = `${amount
      .dividedBy(1000000000)
      .decimalPlaces(1, BigNumber.ROUND_DOWN)}B`;
  } else if (amount.isLessThan(1000000000000000)) {
    formattedAmount = `${amount
      .dividedBy(1000000000000)
      .decimalPlaces(1, BigNumber.ROUND_DOWN)}T`;
  } else {
    formattedAmount = `${amount
      .dividedBy(1000000000000000)
      .decimalPlaces(1, BigNumber.ROUND_DOWN)}Q`;
  }
  return formattedAmount;
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

export const toFixedUserFormat = (number: number, digits: number) => {
  // This is necessary, because the BigNumber version we use
  // can't receive numbers with more than 15 digits
  const parsedNumber = parseFloat(number).toFixed(digits);
  return new BigNumber(parsedNumber).toFormat(digits);
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

export type FormattedDownloadData = {
  timeLeft: string,
  downloaded: string,
  total: string,
  progress: number,
};

export const formattedDownloadData = (
  downloadData?: ?DownloadData,
  userLocale: Locale
): FormattedDownloadData => {
  let timeLeft = '';
  let downloaded = '';
  let total = '';
  let progress = 0;
  if (downloadData) {
    const {
      serverFileSize,
      downloadSize,
      progress: rawProgress,
      speed,
      remainingSize,
    } = downloadData;
    const secondsLeft = remainingSize / speed;
    moment.locale(momentLocales[userLocale]);
    timeLeft = moment().add(secondsLeft, 'seconds').fromNow(true);
    downloaded = formattedBytesToSize(downloadSize);
    total = formattedBytesToSize(serverFileSize);
    progress = parseInt(rawProgress, 10);
  }
  return {
    timeLeft,
    downloaded,
    total,
    progress,
  };
};

export const generateThousands = (value: number) => {
  if (value <= 1000) {
    return Math.round(value);
  }

  return Math.round(value / 1000) * 1000;
};
