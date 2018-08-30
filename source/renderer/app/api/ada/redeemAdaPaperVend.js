// @flow
import type { AdaTransaction, RequestConfig } from './types';
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
  config: RequestConfig,
  { walletPassword, redeemPaperVendedData }: RedeemAdaPaperVendParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/papervend/redemptions/ada',
    port: config.port,
    ca: config.ca,
  }, { passphrase: walletPassword }, redeemPaperVendedData)
);
