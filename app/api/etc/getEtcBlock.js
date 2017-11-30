// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import type { EtcBlock } from './types';

export type GetEtcBlockByHashParams = {
  ca: string,
  blockHash: string,
};

export const getEtcBlockByHash = (
  { ca, blockHash }: GetEtcBlockByHashParams
): Promise<EtcBlock> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'eth_getBlockByHash',
    params: [blockHash, true] // returns the full transaction objects
  })
);
