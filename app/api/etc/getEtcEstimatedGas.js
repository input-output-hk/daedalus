// @flow
import BigNumber from 'bignumber.js';
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import type { EtcGas } from './types';

export type GetEtcEstimatedGasParams = {
  ca: string,
  from: string,
  to: string,
  value: BigNumber, // QUANTITY in WEI with base 10
  gasPrice: BigNumber, // QUANTITY in WEI with base 10
};

export const getEtcEstimatedGas = (
  { ca, from, to, value, gasPrice }: GetEtcEstimatedGasParams
): Promise<EtcGas> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
    ca,
  }, {
    jsonrpc: '2.0',
    method: 'eth_estimateGas',
    params: [{
      from,
      to,
      // Convert quantities to HEX for the ETC api
      value: new BigNumber(value).toString(16),
      gasPrice: new BigNumber(gasPrice).toString(16),
    }]
  })
);
