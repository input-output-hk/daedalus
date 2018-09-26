// @flow
import type { RequestConfig } from '../../common/types';
import type { Transaction } from '../types';
import type { RedeemAdaParams } from './redeemAda';
import { request } from '../../utils/request';

export type RedeemPaperVendedAdaParams = {
  ...RedeemAdaParams,
  mnemonic: Array<string>,
};

export const redeemPaperVendedAda = (
  config: RequestConfig,
  redemptionParams: RedeemPaperVendedAdaParams
): Promise<Transaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions/certificates',
    ...config
  }, {}, redemptionParams)
);
