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
  request(Object.assign({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/redemptions/ada',
  }, config), { passphrase: walletPassword }, walletRedeemData)
);
