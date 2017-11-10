// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import type { EtcTransaction } from './types';

export type GetEtcTransactionByHashResponse = EtcTransaction;

export const getEtcTransactionByHash = (
  ca: string, txHash: string
): Promise<GetEtcTransactionByHashResponse> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'eth_getTransactionByHash',
    params: [txHash]
  })
);
