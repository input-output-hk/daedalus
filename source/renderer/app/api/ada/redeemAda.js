// @flow
import type { AdaTransaction } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

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
    port: environment.WALLET_PORT,
    ca,
  }, { passphrase: walletPassword }, walletRedeemData)
);
