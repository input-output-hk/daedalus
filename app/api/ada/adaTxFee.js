// @flow
import type { ApiTransactionFee } from 'daedalus-client-api';
import { request } from './lib/request';

export type AdaTxFeePathParams = {
  from: string,
  to: string,
  amount: string,
};

export const adaTxFee = (
  ca: string, pathParams: AdaTxFeePathParams
): Promise<ApiTransactionFee> => {
  const { from, to, amount } = pathParams;
  return request({
    hostname: 'localhost',
    method: 'GET',
    path: `/api/txs/fee/${from}/${to}/${amount}`,
    port: 8090,
    ca,
  });
};
