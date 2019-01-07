// @flow
import type { RequestConfig } from '../../common/types';
import type { Transaction } from '../types';
import { request } from '../../utils/request';

export type RedeemAdaParams = {
  redemptionCode: string,
  mnemonic: ?Array<string>,
  spendingPassword?: string,
  walletId: string,
  accountIndex: number
};

export const redeemAda = (
  config: RequestConfig,
  redemptionParams: RedeemAdaParams
): Promise<Transaction> => (
  request({
    method: 'POST',
    path: '/api/v1/transactions/certificates',
    ...config
  }, {}, redemptionParams)
);
