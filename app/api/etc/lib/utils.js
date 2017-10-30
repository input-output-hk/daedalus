// @flow
import { Buffer } from 'safe-buffer';
import { pbkdf2Sync as pbkdf2 } from 'pbkdf2';
import * as unorm from 'unorm';
import BigNumber from 'bignumber.js';
import { isString } from 'lodash';
import { toBigNumber } from 'web3-utils/src/utils';

/**
 * Takes a hex QUANTITY and transforms it into a bignumber
 *
 * @method quantityToBigNumber
 * @param quantity {String} string, quantity as HEX string
 * @return {BigNumber} BigNumber
 */
export const quantityToBigNumber = (quantity: string) => toBigNumber(quantity);

export const mnemonicToSeedHex = (mnemonic: string, password: ?string) => {
  const mnemonicBuffer = Buffer.from(unorm.nfkd(mnemonic), 'utf8');
  const salt = 'mnemonic' + (unorm.nfkd(password) || '');
  const saltBuffer = Buffer.from(salt, 'utf8');
  return pbkdf2(mnemonicBuffer, saltBuffer, 2048, 32, 'sha512').toString('hex');
};

export const unixTimestampToDate = (rawTimestamp: string) => (
  // We have to convert unix timestamp (seconds since …) to
  // JS date (milliseconds since …) by multiplying it with 1000
  new Date(quantityToBigNumber(rawTimestamp).times(1000))
);
