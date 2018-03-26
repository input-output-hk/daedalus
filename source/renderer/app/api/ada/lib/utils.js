// @flow
import { Buffer } from 'safe-buffer';
import { pbkdf2Sync as pbkdf2 } from 'pbkdf2';
import * as unorm from 'unorm';

export const unixTimestampToDate = (timestamp: number) => new Date(timestamp * 1000);

export const mnemonicToSeedHex = (mnemonic: string, password: ?string) => {
  const mnemonicBuffer = Buffer.from(unorm.nfkd(mnemonic), 'utf8');
  const salt = 'mnemonic' + (unorm.nfkd(password) || '');
  const saltBuffer = Buffer.from(salt, 'utf8');
  return pbkdf2(mnemonicBuffer, saltBuffer, 2048, 32, 'sha512').toString('hex');
};
