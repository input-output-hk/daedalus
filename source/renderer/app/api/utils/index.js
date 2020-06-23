// @flow
import moment from 'moment';
import blakejs from 'blakejs';

// time utils
export const unixTimestampToDate = (timestamp: number) =>
  new Date(timestamp * 1000);
export const utcStringToDate = (createDate: string) =>
  moment.utc(createDate).toDate();

// passphrase utils
const bytesToB16 = bytes => Buffer.from(bytes).toString('hex');
const blake2b = data => blakejs.blake2b(data, null, 32);

export const encryptPassphrase = (passphrase: string) =>
  bytesToB16(blake2b(passphrase));

// string utils
export const getContentLength = (content: string) =>
  // 'TextEncoder' is used to measure correct length of UTF-8 strings
  new TextEncoder().encode(content).length;

// legacy wallet ID utils
export const WalletIdPrefix = 'legacy_' | 'hw_';

export const WalletIdPrefixes: {
  LEGACY_WALLET: WalletIdPrefix,
  HARDWARE_WALLET: WalletIdPrefix,
} = {
  LEGACY_WALLET: 'legacy_',
  HARDWARE_WALLET: 'hw_',
};

export const getLegacyWalletId = (rawWalletId: string) =>
  `${WalletIdPrefixes.LEGACY_WALLET}${rawWalletId}`;

export const getHardwareWalletId = (rawWalletId: string) =>
  `${WalletIdPrefixes.HARDWARE_WALLET}${rawWalletId}`;

export const getRawWalletId = (walletId: string, prefix?: string) => {
  const walletIdPrefix = prefix || WalletIdPrefixes.LEGACY_WALLET;
  return walletId.replace(walletIdPrefix, '');
};

export const isHardwareWallet = (walletId: string) =>
  walletId.startsWith(WalletIdPrefixes.HARDWARE_WALLET);
