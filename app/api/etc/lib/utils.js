import BigNumber from 'bignumber.js';
import { isString } from 'lodash';

/**
 * Takes an input and transforms it into an bignumber
 *
 * @method toBigNumber
 * @param number {Number|String|BigNumber} string, HEX string
 * @return {BigNumber} BigNumber
 */
export const toBigNumber = (number) => {
  number = number || 0;

  if (isString(number) && (number.indexOf('0x') === 0 || number.indexOf('-0x') === 0)) {
    return new BigNumber(number.replace('0x',''), 16);
  }

  return new BigNumber(number.toString(10), 10);
};
