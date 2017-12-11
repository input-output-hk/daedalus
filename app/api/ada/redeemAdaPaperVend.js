// @flow
import type { AdaTransaction } from './types';
import { request } from './lib/request';

export type RedeemAdaPaperVendParams = {
  ca: string,
  walletPassword: ?string,
  redeemPaperVendedData: {
    pvWalletId: string,
    pvSeed: string,
    pvBackupPhrase: {
      bpToList: [],
    }
  }
};

export const redeemAdaPaperVend = (
  { ca, walletPassword, redeemPaperVendedData }: RedeemAdaPaperVendParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/papervend/redemptions/ada',
    port: 8090,
    ca,
  }, { passphrase: walletPassword }, redeemPaperVendedData)
);
