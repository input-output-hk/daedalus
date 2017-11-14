// @flow
import type { ApiTransaction } from 'daedalus-client-api';
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
): Promise<ApiTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/papervend/redemptions/ada',
    port: 8090,
    ca,
  }, { passphrase: walletPassword }, redeemPaperVendedData)
);
