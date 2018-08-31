// @flow
import type { AdaTransactionV1 } from './types';
import { request } from './lib/v1/request';
import environment from '../../../../common/environment';

export type RedeemAdaParams = {
  ca: string,
  redemptionCode: string,
  mnemonic: ?Array<string>,
  spendingPassword: string,
  walletId: string,
  accountIndex: number
};

export const redeemAda = (
  { ca, ...rest }: RedeemAdaParams
): Promise<AdaTransactionV1> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions/certificates',
    port: environment.WALLET_PORT,
    ca,
  }, {}, rest)
);
