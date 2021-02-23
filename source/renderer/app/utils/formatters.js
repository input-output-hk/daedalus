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
import type { AssetMetadata } from '../api/assets/types';

export const formattedWalletAmount = (
  amount: BigNumber,
  withCurrency: boolean = true,
  long: boolean = true
) => {
  let formattedAmount = long
    ? new BigNumber(amount).toFormat(DECIMAL_PLACES_IN_ADA)
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
  decimalDigits?: ?number,
  currencySymbol?: ?string
) =>
  `${amount ? amount.times(currencyRate).toFormat(decimalDigits || 2) : 0} ${
    currencySymbol || ''
  }`;

export const formattedTokenWalletAmount = (
  amount: BigNumber,
  metadata?: ?AssetMetadata
) => {
  if (typeof amount === 'number') {
    console.log('HERE!', amount);
    amount = new BigNumber(amount);
  }
  const { acronym, unit } = metadata || {};
  const { decimals } = unit || {};
  let formattedAmount = amount.toFormat(decimals);
  if (acronym) {
    formattedAmount += ` ${acronym}`;
  }
  return formattedAmount;
};

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

export const formattedAmountToBigNumber = (amount: string): BigNumber => {
  const cleanedAmount = amount.replace(/,/g, '');
  return new BigNumber(cleanedAmount !== '' ? cleanedAmount : 0);
};

export const toFixedUserFormat = (number: number, digits: number): string => {
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
  return `${formattedNumber(bytes / 1024 ** i, 1)} ${sizes[i]}`;
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

export const generateThousands = (value: number): number => {
  if (value <= 1000) {
    return Math.round(value);
  }

  return Math.round(value / 1000) * 1000;
};

export const formattedArrayBufferToHexString = (
  arrayBuffer: Uint8Array
): string => {
  const buff = new Uint8Array(arrayBuffer);
  const byteToHex = [];
  const hexOctets = [];

  for (let n = 0; n <= 0xff; ++n) {
    const hexOctet = `0${n.toString(16)}`.slice(-2);
    byteToHex.push(hexOctet);
  }

  for (let i = 0; i < buff.length; ++i) {
    hexOctets.push(byteToHex[buff[i]]);
  }

  return hexOctets.join('');
};

export const formattedNumber = (value: number | string, dp?: number): string =>
  new BigNumber(value).toFormat(dp);

export const formattedCpuModel = (model: string): string => {
  const atCharPosition = model.indexOf('@');
  const speedSection = model.substring(atCharPosition);
  const speedNumbers = speedSection.match(/[\d,.]+/g);
  const speedNumber = speedNumbers ? speedNumbers[0] : '';
  const formattedSpeedNumber = formattedNumber(speedNumber, 2);
  const formattedSpeedSection = speedSection.replace(
    /[\d,.]+/,
    formattedSpeedNumber
  );
  const formattedModel = `${model.substring(
    0,
    atCharPosition
  )}${formattedSpeedSection}`;

  return formattedModel;
};

export const formattedSize = (size: string): string => {
  const sizeNumbers = size.match(/[\d,.]+/g);
  const sizeNumber = sizeNumbers ? sizeNumbers[0] : '';
  const formattedSizeNumber = formattedNumber(sizeNumber);
  const formattedResult = size.replace(/[\d,.]+/, formattedSizeNumber);

  return formattedResult;
};
