// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';

export type CreateEtcAccountParams = [string, string];

export type CreateEtcAccountResponse = string;

export const createEtcAccount = (
  params: CreateEtcAccountParams
): Promise<CreateEtcAccountResponse> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
  }, {
    jsonrpc: '2.0',
    method: 'personal_importRawKey',
    params
  })
);
