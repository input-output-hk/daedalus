'use strict';
var __importDefault =
  (this && this.__importDefault) ||
  function (mod) {
    return mod && mod.__esModule ? mod : { default: mod };
  };
Object.defineProperty(exports, '__esModule', { value: true });
exports.mapToLongDateTimeFormat = exports.getMultiplierFromDecimalPlaces = exports.formattedDateTime = exports.formattedSize = exports.formattedCpuModel = exports.formattedNumber = exports.toFixedWithoutRounding = exports.formattedArrayBufferToHexString = exports.generateThousands = exports.formattedDownloadData = exports.formattedBytesToSize = exports.formattedLovelaceToAmount = exports.formattedAmountToLovelace = exports.toFixedUserFormat = exports.formattedAmountToBigNumber = exports.formattedAmountToNaturalUnits = exports.shortNumber = exports.formattedTokenDecimals = exports.formattedTokenWalletAmount = exports.formattedWalletCurrencyAmount = exports.formattedWalletAmount = void 0;
const bignumber_js_1 = __importDefault(require('bignumber.js'));
const moment_1 = __importDefault(require('moment'));
const numbersConfig_1 = require('../config/numbersConfig');
const assetsConfig_1 = require('../config/assetsConfig');
const profileConfig_1 = require('../config/profileConfig');
const locales_types_1 = require('../../../common/types/locales.types');
const formattedWalletAmount = (
  amount,
  withCurrency = true,
  long = true,
  currency = 'ADA',
  decimalPlaces = numbersConfig_1.DECIMAL_PLACES_IN_ADA
) => {
  let formattedAmount = long
    ? new bignumber_js_1.default(amount).toFormat(decimalPlaces)
    : (0, exports.shortNumber)(amount, decimalPlaces);
  // @ts-ignore ts-migrate(2554) FIXME: Expected 1 arguments, but got 0.
  const { decimalSeparator } = bignumber_js_1.default.config().FORMAT;
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
exports.formattedWalletAmount = formattedWalletAmount;
const formattedWalletCurrencyAmount = (
  amount,
  currencyRate,
  decimalDigits,
  currencyCode
) =>
  `${amount ? amount.times(currencyRate).toFormat(decimalDigits || 2) : 0} ${
    currencyCode || ''
  }`;
exports.formattedWalletCurrencyAmount = formattedWalletCurrencyAmount;
const formattedTokenWalletAmount = (
  amount,
  metadata,
  // @ts-ignore ts-migrate(1016) FIXME: A required parameter cannot follow an optional par... Remove this comment to see the full error message
  decimals,
  isShort
) => {
  const { ticker } = metadata || {};
  let formattedAmount = (0, exports.formattedTokenDecimals)(amount, decimals);
  if (isShort) {
    if (formattedAmount.isGreaterThanOrEqualTo(1000)) {
      /*
       * Short formatting for >= 1000
       * E.G.: 1,000,000 prints '1M'
       */
      // @ts-ignore ts-migrate(2322) FIXME: Type 'string' is not assignable to type 'BigNumber... Remove this comment to see the full error message
      formattedAmount = (0, exports.shortNumber)(formattedAmount);
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
      formattedAmount = (0, exports.toFixedWithoutRounding)(
        formattedAmount.toFormat(),
        2
      );
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
exports.formattedTokenWalletAmount = formattedTokenWalletAmount;
const formattedTokenDecimals = (amount, decimals) => {
  const decimalPrecision = decimals || assetsConfig_1.DEFAULT_DECIMAL_PRECISION;
  const divider = parseInt(
    (0, exports.getMultiplierFromDecimalPlaces)(decimalPrecision),
    10
  );
  return amount.dividedBy(divider);
};
exports.formattedTokenDecimals = formattedTokenDecimals;
// Symbol   Name                Scientific Notation
// K        Thousand            1.00E+03
// M        Million             1.00E+06
// B        Billion             1.00E+09
// T        Trillion            1.00E+12
// Q        Quadrillion         1.00E+15
const shortNumber = (
  value,
  decimalPlaces = numbersConfig_1.DECIMAL_PLACES_IN_ADA
) => {
  const amount = new bignumber_js_1.default(value);
  let formattedAmount = '';
  if (amount.isZero()) {
    formattedAmount = '0';
  } else if (amount.isLessThan(1000)) {
    formattedAmount = `${amount.decimalPlaces(
      decimalPlaces,
      bignumber_js_1.default.ROUND_DOWN
    )}`;
  } else if (amount.isLessThan(1000000)) {
    formattedAmount = `${amount
      .dividedBy(1000)
      .decimalPlaces(1, bignumber_js_1.default.ROUND_DOWN)}K`;
  } else if (amount.isLessThan(1000000000)) {
    formattedAmount = `${amount
      .dividedBy(1000000)
      .decimalPlaces(1, bignumber_js_1.default.ROUND_DOWN)}M`;
  } else if (amount.isLessThan(1000000000000)) {
    formattedAmount = `${amount
      .dividedBy(1000000000)
      .decimalPlaces(1, bignumber_js_1.default.ROUND_DOWN)}B`;
  } else if (amount.isLessThan(1000000000000000)) {
    formattedAmount = `${amount
      .dividedBy(1000000000000)
      .decimalPlaces(1, bignumber_js_1.default.ROUND_DOWN)}T`;
  } else {
    formattedAmount = `${amount
      .dividedBy(1000000000000000)
      .decimalPlaces(1, bignumber_js_1.default.ROUND_DOWN)}Q`;
  }
  return formattedAmount;
};
exports.shortNumber = shortNumber;
const formattedAmountToNaturalUnits = (amount) => {
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
exports.formattedAmountToNaturalUnits = formattedAmountToNaturalUnits;
const formattedAmountToBigNumber = (amount) => {
  const cleanedAmount = amount.replace(/,/g, '');
  return new bignumber_js_1.default(cleanedAmount !== '' ? cleanedAmount : 0);
};
exports.formattedAmountToBigNumber = formattedAmountToBigNumber;
const toFixedUserFormat = (number, digits) => {
  // This is necessary, because the BigNumber version we use
  // can't receive numbers with more than 15 digits
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
  const parsedNumber = parseFloat(number).toFixed(digits);
  return new bignumber_js_1.default(parsedNumber).toFormat(digits);
};
exports.toFixedUserFormat = toFixedUserFormat;
const formattedAmountToLovelace = (amount) =>
  // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BigNumber' is not assignable to ... Remove this comment to see the full error message
  parseInt(
    (0, exports.formattedAmountToBigNumber)(amount).times(
      numbersConfig_1.LOVELACES_PER_ADA
    ),
    10
  );
exports.formattedAmountToLovelace = formattedAmountToLovelace;
const formattedLovelaceToAmount = (lovelace) =>
  (0, exports.formattedAmountToBigNumber)(String(lovelace))
    .dividedBy(numbersConfig_1.LOVELACES_PER_ADA)
    .toNumber();
exports.formattedLovelaceToAmount = formattedLovelaceToAmount;
const formattedBytesToSize = (bytes, decimalPlaces = 1) => {
  const sizes = ['Bytes', 'KB', 'MB', 'GB', 'TB'];
  if (bytes === 0) return 'n/a';
  const i = parseInt(
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'number' is not assignable to par... Remove this comment to see the full error message
    Math.floor(Math.log(Math.abs(bytes)) / Math.log(1024)),
    10
  );
  if (i === 0) return `${bytes} ${sizes[i]})`;
  return `${(0, exports.formattedNumber)(bytes / 1024 ** i, decimalPlaces)} ${
    sizes[i]
  }`;
};
exports.formattedBytesToSize = formattedBytesToSize;
const formattedDownloadData = (
  downloadData,
  // @ts-ignore ts-migrate(1016) FIXME: A required parameter cannot follow an optional par... Remove this comment to see the full error message
  userLocale
) => {
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
    moment_1.default.locale(locales_types_1.momentLocales[userLocale]);
    timeLeft = (0, moment_1.default)()
      .add(secondsLeft, 'seconds')
      .fromNow(true);
    downloaded = (0, exports.formattedBytesToSize)(downloadSize);
    total = (0, exports.formattedBytesToSize)(serverFileSize);
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
exports.formattedDownloadData = formattedDownloadData;
const generateThousands = (value) => {
  if (value <= 1000) {
    return Math.round(value);
  }
  return Math.round(value / 1000) * 1000;
};
exports.generateThousands = generateThousands;
const formattedArrayBufferToHexString = (arrayBuffer) => {
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
exports.formattedArrayBufferToHexString = formattedArrayBufferToHexString;
const toFixedWithoutRounding = (num, fixed) => {
  const re = new RegExp(`^-?\\d+(?:.\\d{0,${fixed || -1}})?`);
  const results = num.toString().match(re);
  return results && results.length ? results[0].toString() : '0';
};
exports.toFixedWithoutRounding = toFixedWithoutRounding;
const formattedNumber = (value, dp) =>
  new bignumber_js_1.default(value).toFormat(dp);
exports.formattedNumber = formattedNumber;
const formattedCpuModel = (model) => {
  const atCharPosition = model.indexOf('@');
  const speedSection = model.substring(atCharPosition);
  const speedNumbers = speedSection.match(/[\d,.]+/g);
  const speedNumber = speedNumbers ? speedNumbers[0] : '';
  const formattedSpeedNumber = (0, exports.formattedNumber)(speedNumber, 2);
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
exports.formattedCpuModel = formattedCpuModel;
const formattedSize = (size) => {
  const sizeNumbers = size.match(/[\d,.]+/g);
  const sizeNumber = sizeNumbers ? sizeNumbers[0] : '';
  const formattedSizeNumber = (0, exports.formattedNumber)(sizeNumber);
  const formattedResult = size.replace(/[\d,.]+/, formattedSizeNumber);
  return formattedResult;
};
exports.formattedSize = formattedSize;
const formattedDateTime = (
  dateTime,
  { currentLocale, currentDateFormat, currentTimeFormat }
) => {
  moment_1.default.locale(locales_types_1.momentLocales[currentLocale]);
  const dateTimeMoment = (0, moment_1.default)(dateTime);
  const dateFormatted = dateTimeMoment.format(currentDateFormat);
  if (currentTimeFormat) {
    const timeFormatted = dateTimeMoment.format(currentTimeFormat);
    const dateTimeSeparator =
      profileConfig_1.DATE_TIME_SEPARATOR_MAP[currentDateFormat];
    return `${dateFormatted}${dateTimeSeparator}${timeFormatted}`;
  }
  return dateFormatted;
};
exports.formattedDateTime = formattedDateTime;
const getMultiplierFromDecimalPlaces = (decimalPlaces) =>
  '1'.padEnd(decimalPlaces + 1, '0');
exports.getMultiplierFromDecimalPlaces = getMultiplierFromDecimalPlaces;
const mapToLongDateTimeFormat = ({
  currentLocale,
  currentDateFormat,
  currentTimeFormat,
}) => {
  const mappedDateFormat =
    currentLocale === locales_types_1.LOCALES.english
      ? profileConfig_1.DATE_ENGLISH_LL_MAP_OPTIONS[currentDateFormat]
      : currentDateFormat;
  return {
    currentDateFormat: mappedDateFormat,
    currentTimeFormat: profileConfig_1.TIME_LL_MAP_OPTIONS[currentTimeFormat],
  };
};
exports.mapToLongDateTimeFormat = mapToLongDateTimeFormat;
//# sourceMappingURL=formatters.js.map
