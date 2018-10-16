// @flow
import type { AdaLocalTimeDifference } from './types';
import { request } from './lib/request';
import environment from '../../../../common/environment';

export type GetAdaLocalTimeDifferenceParams = {
  ca: string,
};

export const getAdaLocalTimeDifference = (
  { ca }: GetAdaLocalTimeDifferenceParams
): Promise<AdaLocalTimeDifference> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/settings/time/difference',
    port: environment.WALLET_PORT,
    ca,
  })
);
