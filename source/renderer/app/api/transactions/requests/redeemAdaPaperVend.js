// @flow
// import RequestConfig
import type { AdaTransaction } from '../types';
import type { RedeemAdaParams } from './redeemAda';
import { request } from '../../utils/request';

export type RedeemPaperVendedAdaParams = {
  mnemonic: Array<string>,
  ...RedeemAdaParams
};

export const redeemAdaPaperVend = (
  config: RequestConfig,
  redemptionParams: RedeemPaperVendedAdaParams
): Promise<AdaTransaction> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions/certificates',
    ...config
  }, {}, redemptionParams)
);
