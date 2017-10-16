// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';

export type GetEtcAccountsResponse = {
  result: Array<string>,
};

export const getEtcAccounts = (): Promise<GetEtcAccountsResponse> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
  }, {
    jsonrpc: '2.0',
    method: 'personal_listAccounts',
  })
);
