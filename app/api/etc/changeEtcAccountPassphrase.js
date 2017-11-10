// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';

export type ChangeEtcAccountPassphraseParams = {
  address: string,
  oldPassphrase: string,
  newPassphrase: string,
};

export type ChangeEtcAccountPassphraseResponse = '';

export const changeEtcAccountPassphrase = (
  ca: string, { address, oldPassphrase, newPassphrase }: ChangeEtcAccountPassphraseParams
): Promise<ChangeEtcAccountPassphraseResponse> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'daedalus_changePassphrase',
    params: [address, oldPassphrase, newPassphrase]
  })
);
