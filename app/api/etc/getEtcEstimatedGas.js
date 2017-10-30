// @flow
import { request } from './lib/request';
import { ETC_API_HOST, ETC_API_PORT } from './index';
import BigNumber from 'bignumber.js';

export type GetEtcEstimatedGasParams = {
  from: string,
  to: string,
  value: BigNumber, // QUANTITY in WEI with base 10
  gasPrice: BigNumber, // QUANTITY in WEI with base 10
};

export type GetEtcEstimatedGasResponse = string;

export const getEtcEstimatedGas = (
  params: GetEtcEstimatedGasParams
): Promise<GetEtcEstimatedGasResponse> => (
  request({
    hostname: ETC_API_HOST,
    method: 'POST',
    path: '/',
    port: ETC_API_PORT,
  }, {
    jsonrpc: '2.0',
    method: 'eth_estimateGas',
    params: [{
      ...params,
      // Convert quantities to HEX for the ETC api
      value: new BigNumber(params.value).toString(16),
      gasPrice: new BigNumber(params.gasPrice).toString(16),
    }]
  })
);
