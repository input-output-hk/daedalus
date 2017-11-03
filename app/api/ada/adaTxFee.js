// @flow
import type { ApiTransactionFee } from 'daedalus-client-api';
import { get } from 'lodash';
import { request } from './lib/request';

export type AdaTxFeePathParams = {
  from: string,
  to: string,
  amount: string,
};

export type AdaTxFeeQueryParams = {};

export type AdaTxFeeRawBodyParams = ?{
  inputSelectionPolicy: {
    // "OptimizeForSecurity" - Spend everything from the address
    // "OptimizeForSize" for no grouping
    groupingPolicy: 'OptimizeForSecurity' | 'OptimizeForSize',
  }
};

export const adaTxFee = (
  ca: string,
  pathParams: AdaTxFeePathParams,
  queryParams: AdaTxFeeQueryParams,
  rawBodyParams: AdaTxFeeRawBodyParams
): Promise<ApiTransactionFee> => {
  const { from, to, amount } = pathParams;
  const inputSelectionPolicy = get(rawBodyParams, 'inputSelectionPolicy', null);
  return request({
    hostname: 'localhost',
    method: 'POST',
    path: `/api/txs/fee/${from}/${to}/${amount}`,
    port: 8090,
    ca,
  }, queryParams, inputSelectionPolicy);
};
