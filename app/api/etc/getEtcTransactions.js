// @flow
import BigNumber from 'bignumber.js';
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import type { EtcTransaction } from './types';

export type GetEtcTransactionsParams = {
  ca: string,
  accountAddress: string,
  fromBlock: number,
  toBlock: number,
};

export type GetEtcTransactionsResponse = {
  received: Array<EtcTransaction>,
  sent: Array<EtcTransaction>,
};

/**
 * Returns account transactions (both sent and received) from a range of blocks.
 * The response also includes pending transactions.
 * @param ca (the TLS certificate)
 * @param accountAddress
 * @param fromBlock (in the past)
 * @param toBlock (more recent)
 * @returns {*}
 */
export const getEtcTransactionsForAccount = (
  { ca, accountAddress, fromBlock, toBlock }: GetEtcTransactionsParams
): Promise<GetEtcTransactionsResponse> => {
  const params = [
    accountAddress,
    new BigNumber(fromBlock).toString(16),
    new BigNumber(toBlock).toString(16),
  ];
  return (
    request({
      hostname: ETC_API_HOST,
      method: 'POST',
      path: '/',
      port: ETC_API_PORT,
      ca,
    }, {
      jsonrpc: '2.0',
      method: 'daedalus_getAccountTransactions',
      params,
    })
  );
};
