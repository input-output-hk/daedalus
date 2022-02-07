import BigNumber from 'bignumber.js';
import moment from 'moment';
import {
  DECIMAL_PLACES_IN_ADA,
  LOVELACES_PER_ADA,
} from '../config/numbersConfig';
import { DEFAULT_DECIMAL_PRECISION } from '../config/assetsConfig';
import {
  DATE_ENGLISH_LL_MAP_OPTIONS,
  TIME_LL_MAP_OPTIONS,
  DATE_TIME_SEPARATOR_MAP,
} from '../config/profileConfig';
import { momentLocales, LOCALES } from '../../../common/types/locales.types';
import type { DownloadData } from '../../../common/types/downloadManager.types';
import type { Locale } from '../../../common/types/locales.types';
import type { AssetMetadata } from '../api/assets/types';

export const formattedWalletAmount = (
  amount: BigNumber,
  withCurrency = true,
  long = true,
  currency = 'ADA'
): string => {
  let formattedAmount = long
    ? new BigNumber(amount).toFormat(DECIMAL_PLACES_IN_ADA)
    : shortNumber(amount);
  // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
  const { decimalSeparator } = BigNumber.config().FORMAT;

  if (!long && decimalSeparator !== '.') {
    // Only BigNumber.toFormat() method is applying correct separators.
    // Since this method is not used for condensed format (long = false)
    // the correct number format has to be applied manually.
    formattedAmount = formattedAmount.split('.').join(decimalSeparator);
  }

  if (withCurrency) {
    formattedAmount = `${formattedAmount} ${currency}`;
  }

  return formattedAmount.toString();
};
export const formattedWalletCurrencyAmount = (
  amount: BigNumber,
  currencyRate: number,
  decimalDigits?: number | null | undefined,
  currencyCode?: string | null | undefined
): string =>
  `${amount ? amount.times(currencyRate).toFormat(decimalDigits || 2) : 0} ${
    currencyCode || ''
  }`;
export const formattedTokenWalletAmount = (
  amount: BigNumber,
  metadata?: AssetMetadata | null | undefined,
  // @ts-ignore ts-migrate(1016) FIXME: A required parameter cannot follow an optional par... Remove this comment to see the full error message
  decimals: number | null | undefined,
  isShort?: boolean
): string => {
  const { ticker } = metadata || {};
  let formattedAmount = formattedTokenDecimals(amount, decimals);

  if (isShort) {
    if (formattedAmount.isGreaterThanOrEqualTo(1000)) {
      /*
       * Short formatting for >= 1000
       * E.G.: 1,000,000 prints '1M'
       */
      // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'BigNumber... Remove this comment to see the full error message
      formattedAmount = shortNumber(formattedAmount);
    } else if (formattedAmount.isZero()) {
      return '0';
    } else if (formattedAmount.isLessThan(0.01)) {
      /*
       * Short formatting for < 0.01
       * E.G.: 0.000009 prints '< 0.01'
       */
      // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'BigNumber... Remove this comment to see the full error message
      formattedAmount = '< 0.01';
    } else {
      /*
       * Short formatting for < 1000 & > 0.01
       * E.G.: 0.999999 prints '0.99'
       */
      // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'BigNumber... Remove this comment to see the full error message
      formattedAmount = toFixedWithoutRounding(formattedAmount.toFormat(), 2);
    }
  } else {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'BigNumber... Remove this comment to see the full error message
    formattedAmount = formattedAmount.toFormat(decimals);
  }

  if (ticker) {
    // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'BigNumber... Remove this comment to see the full error message
    formattedAmount += ` ${ticker}`;
  }

  // @ts-ignore ts-migrate(2322) FIXME: Type 'BigNumber' is not assignable to type 'string... Remove this comment to see the full error message
  return formattedAmount;
};
export const formattedTokenDecimals = (
  amount: BigNumber,
  decimals: number | null | undefined
): BigNumber => {
  const decimalPrecision = decimals || DEFAULT_DECIMAL_PRECISION;
  const divider = parseInt(
    getMultiplierFromDecimalPlaces(decimalPrecision),
    10
  );
  return amount.dividedBy(divider);
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
export const formattedAmountToNaturalUnits = (
  amount: string | null | undefined
): string => {
  if (!amount) {
    return '0';
  }

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
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
  const parsedNumber = parseFloat(number).toFixed(digits);
  return new BigNumber(parsedNumber).toFormat(digits);
};
export const formattedAmountToLovelace = (amount: string): number =>
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BigNumber' is not assignable to ... Remove this comment to see the full error message
  parseInt(formattedAmountToBigNumber(amount).times(LOVELACES_PER_ADA), 10);
export const formattedLovelaceToAmount = (lovelace: number): number =>
  formattedAmountToBigNumber(String(lovelace))
    .dividedBy(LOVELACES_PER_ADA)
    .toNumber();
export const formattedBytesToSize = (bytes: number): string => {
  const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB'];
  if (bytes === 0) return 'n/a';
  const i = parseInt(
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
    Math.floor(Math.log(Math.abs(bytes)) / Math.log(1024)),
    10
  );
  if (i === 0) return `${bytes} ${sizes[i]})`;
  return `${formattedNumber(bytes / 1024 ** i, 1)} ${sizes[i]}`;
};
export type FormattedDownloadData = {
  timeLeft: string;
  downloaded: string;
  total: string;
  progress: number;
};
export const formattedDownloadData = (
  downloadData?: DownloadData | null | undefined,
  // @ts-ignore ts-migrate(1016) FIXME: A required parameter cannot follow an optional par... Remove this comment to see the full error message
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
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
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
export const toFixedWithoutRounding = (
  num: number | string,
  fixed: number
): string => {
  const re = new RegExp(`^-?\\d+(?:.\\d{0,${fixed || -1}})?`);
  const results = num.toString().match(re);
  return results && results.length ? results[0].toString() : '0';
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
type CurrentFormats = {
  currentLocale: Locale;
  currentDateFormat: string;
  currentTimeFormat?: string;
};
export const formattedDateTime = (
  dateTime: Date,
  { currentLocale, currentDateFormat, currentTimeFormat }: CurrentFormats
) => {
  moment.locale(momentLocales[currentLocale]);
  const dateTimeMoment = moment(dateTime);
  const dateFormatted = dateTimeMoment.format(currentDateFormat);

  if (currentTimeFormat) {
    const timeFormatted = dateTimeMoment.format(currentTimeFormat);
    const dateTimeSeparator = DATE_TIME_SEPARATOR_MAP[currentDateFormat];
    return `${dateFormatted}${dateTimeSeparator}${timeFormatted}`;
  }

  return dateFormatted;
};
export const getMultiplierFromDecimalPlaces = (decimalPlaces: number) =>
  '1'.padEnd(decimalPlaces + 1, '0');
export const mapToLongDateTimeFormat = ({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
}: CurrentFormats) => {
  const mappedDateFormat =
    currentLocale === LOCALES.english
      ? DATE_ENGLISH_LL_MAP_OPTIONS[currentDateFormat]
      : currentDateFormat;
  return {
    currentDateFormat: mappedDateFormat,
    currentTimeFormat: TIME_LL_MAP_OPTIONS[currentTimeFormat],
  };
};
