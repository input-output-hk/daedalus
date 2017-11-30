// @flow
import type { AdaTransaction } from './types';
import { request } from './lib/request';

export type RedeemAdaParams = {
  ca: string,
  walletPassword: ?string,
  walletRedeemData: {
    crWalletId: string,
    crSeed: string,
  }
};

export const redeemAda = (
{ ca, walletPassword, walletRedeemData }: RedeemAdaParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/redemptions/ada',
    port: 8090,
    ca,
  }, { passphrase: walletPassword }, walletRedeemData)
);
