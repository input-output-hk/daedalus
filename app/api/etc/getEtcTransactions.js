// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';

export type GetEtcTransactionsParams = {
  accountAddress: string,
};

export type GetEtcTransactionsResponse = Array<{
  hash: string,
  nonce: string,
  blockHash: string,
  blockNumber: string,
  transactionIndex: string,
  from: string,
  to: string,
  value: string,
  gasPrice: string,
  gas: string,
  input: string,
}>;

export const getEtcTransactionsForAccount = (
  params: GetEtcTransactionsParams
): Promise<GetEtcTransactionsResponse> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
  }, {
    jsonrpc: '2.0',
    method: 'eth_getTransactions',
    params: [params.accountAddress]
  })
);
