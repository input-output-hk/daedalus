// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import type { EtcTransaction } from './types';

export type GetEtcTransactionsParams = {
  accountAddress: string,
};

export type GetEtcTransactionsResponse = {
  received: Array<EtcTransaction>,
  sent: Array<EtcTransaction>,
};

export const getEtcTransactionsForAccount = (
  ca: string, params: GetEtcTransactionsParams
): Promise<GetEtcTransactionsResponse> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'daedalus_getAccountRecentTransactions',
    params: [params.accountAddress]
  })
);
