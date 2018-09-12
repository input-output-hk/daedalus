// @flow
// import RequestConfig
import type { AdaTransaction } from '../types';
import { request } from '../../utils/request';

export type RedeemAdaParams = {
  redemptionCode: string,
  mnemonic: ?Array<string>,
  spendingPassword: string,
  walletId: string,
  accountIndex: number
};

export const redeemAda = (
  config: RequestConfig,
  redemptionParams: RedeemAdaParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions/certificates',
    ...config
  }, {}, redemptionParams)
);
