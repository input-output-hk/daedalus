// @flow
import { Buffer } from 'safe-buffer';
import { pbkdf2Sync as pbkdf2 } from 'pbkdf2';
import * as unorm from 'unorm';
import BigNumber from 'bignumber.js';
import { isString } from 'lodash';

/**
 * Takes an input and transforms it into an bignumber
 *
 * @method toBigNumber
 * @param number {Number|String|BigNumber} string, HEX string
 * @return {BigNumber} BigNumber
 */
export const quantityToBigNumber = (number: string) => {
  number = number || '0';
  if (isString(number) && (number.indexOf('0x') === 0 || number.indexOf('-0x') === 0)) {
    return new BigNumber(number, 16);
  }
  return new BigNumber(number, 10);
};

export const mnemonicToSeedHex = (mnemonic: string, password: ?string) => {
  const mnemonicBuffer = Buffer.from(unorm.nfkd(mnemonic), 'utf8');
  const salt = 'mnemonic' + (unorm.nfkd(password) || '');
  const saltBuffer = Buffer.from(salt, 'utf8');
  return pbkdf2(mnemonicBuffer, saltBuffer, 2048, 32, 'sha512').toString('hex');
};

export const unixTimestampToDate = (rawTimestamp: string) => (
  // We have to convert unix timestamp (seconds since …) to
  // JS date (milliseconds since …) by multiplying it with 1000
  new Date(quantityToBigNumber(rawTimestamp).times(1000).toNumber())
);
