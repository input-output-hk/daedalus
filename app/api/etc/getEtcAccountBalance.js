// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';


export type GetEtcAccountBalanceParams = {
  ca: string,
  accountId: string,
  status: 'latest' | 'earliest' | 'pending',
};

export type GetEtcAccountBalanceResponse = string;

export const getEtcAccountBalance = (
  { ca, accountId, status }: GetEtcAccountBalanceParams
): Promise<GetEtcAccountBalanceResponse> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'eth_getBalance',
    params: [accountId, status]
  })
);
