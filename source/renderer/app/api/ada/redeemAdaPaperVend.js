// @flow
import type { AdaTransaction } from './types';
import { request } from './lib/request';

export type RedeemAdaPaperVendParams = {
  ca: string,
  port: number,
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
  { ca, port, walletPassword, redeemPaperVendedData }: RedeemAdaPaperVendParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/papervend/redemptions/ada',
    port,
    ca,
  }, { passphrase: walletPassword }, redeemPaperVendedData)
);
