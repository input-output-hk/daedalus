// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import type { EtcAccounts } from './types';

export type GetEtcAccountsParams = {
  ca: string,
};

export const getEtcAccounts = (
  { ca }: GetEtcAccountsParams
): Promise<EtcAccounts> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'personal_listAccounts',
  })
);
