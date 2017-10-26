// @flow
import type { ApiTransaction } from 'daedalus-client-api';
import { request } from './lib/request';

export type NewPaymentPathParams = {
  from: string,
  to: string,
  amount: string,
};

export type NewPaymentQueryParams = {
  passphrase: ?string,
};

export const newAdaPayment = (
  ca: string, pathParams: NewPaymentPathParams, queryParams: NewPaymentQueryParams
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
