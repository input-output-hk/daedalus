// @flow
import type { AdaTransaction } from './types';
import { request } from './lib/request';

export type RedeemAdaParams = {
  ca: string,
  port: number,
  walletPassword: ?string,
  walletRedeemData: {
    crWalletId: string,
    crSeed: string,
  }
};

export const redeemAda = (
  { ca, port, walletPassword, walletRedeemData }: RedeemAdaParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/redemptions/ada',
    port,
    ca,
  }, { passphrase: walletPassword }, walletRedeemData)
);
