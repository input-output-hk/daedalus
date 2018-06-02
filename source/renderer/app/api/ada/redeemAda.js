// @flow
import type { AdaTransaction } from './types';
import { request } from './lib/request';

export type RedeemAdaParams = {
  walletPassword: ?string,
  walletRedeemData: {
    crWalletId: string,
    crSeed: string,
  }
};

export const redeemAda = (
  { walletPassword, walletRedeemData }: RedeemAdaParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/redemptions/ada',
  }, { passphrase: walletPassword }, walletRedeemData)
);
