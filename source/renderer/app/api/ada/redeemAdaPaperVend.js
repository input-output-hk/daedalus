// @flow
import type { AdaTransaction } from './types';
import { request } from './lib/request';

export type RedeemAdaPaperVendParams = {
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
  { walletPassword, redeemPaperVendedData }: RedeemAdaPaperVendParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/papervend/redemptions/ada',
  }, { passphrase: walletPassword }, redeemPaperVendedData)
);
