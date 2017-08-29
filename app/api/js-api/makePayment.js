// @flow
import { request } from './lib/request';
import type { ApiTransaction } from 'daedalus-client-api';

export type MakePaymentPathParams = {
  from: string,
  to: string,
  amount: string,
};

export type MakePaymentQueryParams = {
  passphrase: ?string,
};

export const makePayment = (
  ca: string, pathParams: MakePaymentPathParams, queryParams: MakePaymentQueryParams
): Promise<ApiTransaction> => {
  const { from, to, amount } = pathParams;
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/txs/payments/${from}/${to}/${amount}`,
    port: 8090,
    ca,
  }, queryParams);
};
