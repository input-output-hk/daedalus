// @flow
import BigNumber from 'bignumber.js';
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import type { EtcTransactions } from './types';

export type GetEtcTransactionsParams = {
  ca: string,
  walletId: string,
  fromBlock: number,
  toBlock: number,
};

/**
 * Returns account transactions (both sent and received) from a range of blocks.
 * The response also includes pending transactions.
 * @param ca (the TLS certificate)
 * @param walletId
 * @param fromBlock (in the past)
 * @param toBlock (more recent)
 * @returns {*}
 */
export const getEtcTransactions = (
  { ca, walletId, fromBlock, toBlock }: GetEtcTransactionsParams
): Promise<EtcTransactions> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'daedalus_getAccountTransactions',
    params: [
      walletId,
      new BigNumber(fromBlock).toString(16),
      new BigNumber(toBlock).toString(16),
    ],
  })
);
