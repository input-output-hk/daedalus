// @flow
import type { ApiTransaction } from 'daedalus-client-api';
import { get } from 'lodash';
import { request } from './lib/request';

export type NewPaymentPathParams = {
  from: string,
  to: string,
  amount: string,
};

export type NewPaymentQueryParams = {
  passphrase: ?string,
};

export type NewPaymentRawBodyParams = ?{
  inputSelectionPolicy: {
    // "OptimizeForSecurity" - Spend everything from the address
    // "OptimizeForSize" for no grouping
    groupingPolicy: 'OptimizeForSecurity' | 'OptimizeForSize',
  }
};

export const newAdaPayment = (
  ca: string,
  pathParams: NewPaymentPathParams,
  queryParams: NewPaymentQueryParams,
  rawBodyParams: NewPaymentRawBodyParams
): Promise<ApiTransaction> => {
  const { from, to, amount } = pathParams;
  const inputSelectionPolicy = get(rawBodyParams, 'inputSelectionPolicy', null);
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/txs/payments/${from}/${to}/${amount}`,
    port: 8090,
    ca,
  }, queryParams, inputSelectionPolicy);
};
