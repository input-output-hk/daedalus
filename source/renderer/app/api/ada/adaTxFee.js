// @flow
import type { AdaTransactionFee, AdaTransactionPayloadV1 } from './types';
import { request } from './lib/v1/request';

export const adaTxFee = (
  { ca, data }: AdaTransactionPayloadV1
): Promise<AdaTransactionFee> => (
  request({
    hostname: 'localhost',
    method: 'POST',
    path: '/api/v1/transactions/fees',
    port: 8090,
    ca,
  }, {}, data)
);
