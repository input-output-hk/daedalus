// @flow
import BigNumber from 'bignumber.js';
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import type { EtcTxHash } from './types';

export type SendEtcTransactionParams = {
  ca: string,
  from: string,
  to: string,
  value: BigNumber,
  password: string,
  gasPrice: BigNumber,
};

export const sendEtcTransaction = (
  { ca, from, to, value, password, gasPrice }: SendEtcTransactionParams
): Promise<EtcTxHash> => {
  const txParams = {
    from,
    to,
    value: value.toString(16),
    gasPrice: gasPrice.toString(16),
  };
  return (
    request({
      hostname: ETC_API_HOST,
      method: 'POST',
      path: '/',
      port: ETC_API_PORT,
      ca,
    }, {
      jsonrpc: '2.0',
      method: 'personal_sendTransaction',
      params: [
        txParams,
        password,
      ]
    })
  );
};
