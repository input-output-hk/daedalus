// @flow
import BigNumber from 'bignumber.js';

export const formattedNumber = (value: number | string, dp?: number): string =>
  new BigNumber(value).toFormat(dp);

export const formattedSize = (size: string): string => {
  const sizeNumbers = size.match(/[\d,.]+/g);
  const sizeNumber = sizeNumbers ? sizeNumbers[0] : '';
  const formattedSizeNumber = formattedNumber(sizeNumber);
  const formattedResult = size.replace(/[\d,.]+/, formattedSizeNumber);

  return formattedResult;
};
