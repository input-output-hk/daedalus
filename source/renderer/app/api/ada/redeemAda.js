// @flow
import type { AdaTransaction, RequestConfig } from './types';
import { request } from './lib/request';

export type RedeemAdaParams = {
  walletPassword: ?string,
  walletRedeemData: {
    crWalletId: string,
    crSeed: string,
  }
};

export const redeemAda = (
  config: RequestConfig,
  { walletPassword, walletRedeemData }: RedeemAdaParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/redemptions/ada',
    port: config.port,
    ca: config.ca,
  }, { passphrase: walletPassword }, walletRedeemData)
);
