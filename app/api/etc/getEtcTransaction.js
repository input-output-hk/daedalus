// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import type { EtcTransaction } from './types';

export type GetEtcTransactionByHashParams = {
  ca: string,
  txHash: string
};

export const getEtcTransactionByHash = (
  { ca, txHash }: GetEtcTransactionByHashParams
): Promise<EtcTransaction> => (
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
