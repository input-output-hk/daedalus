// @flow
import type { AdaTransaction } from './types';
import { request } from './lib/request';

export type RedeemAdaParams = {
  apiParams: {
    ca: string,
    port: number,
    clientCert: string,
    clientKey: string,
  },
  walletPassword: ?string,
  walletRedeemData: {
    crWalletId: string,
    crSeed: string,
  }
};

export const redeemAda = (
  { apiParams, walletPassword, walletRedeemData }: RedeemAdaParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/redemptions/ada',
    port: apiParams.port,
    ca: apiParams.ca,
    cert: apiParams.clientCert,
    key: apiParams.clientKey,
  }, { passphrase: walletPassword }, walletRedeemData)
);
