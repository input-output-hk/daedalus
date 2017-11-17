// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';

export type DeleteEtcAccountBalanceResponse = boolean;

export const deleteEtcAccount = (
  ca: string, accountId: string
): Promise<DeleteEtcAccountBalanceResponse> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'daedalus_deleteWallet',
    params: [accountId]
  })
);
