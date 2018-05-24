// @flow
import type { AdaTransaction } from './types';
import { request } from './lib/request';

export type RedeemAdaPaperVendParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
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
  { apiParams, walletPassword, redeemPaperVendedData }: RedeemAdaPaperVendParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/papervend/redemptions/ada',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, { passphrase: walletPassword }, redeemPaperVendedData)
);
