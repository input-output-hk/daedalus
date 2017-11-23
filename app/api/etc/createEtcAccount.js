// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';

export type CreateEtcAccountParams = {
  ca: string,
  privateKey: string,
  password: ?string,
};

export type CreateEtcAccountResponse = string;

export const createEtcAccount = (
  { ca, privateKey, password }: CreateEtcAccountParams
): Promise<CreateEtcAccountResponse> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'personal_importRawKey',
    params: [
      privateKey,
      password || '',
    ]
  })
);
