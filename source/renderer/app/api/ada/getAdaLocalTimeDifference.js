// @flow
import type { AdaLocalTimeDifference } from './types';
import { request } from './lib/request';

export type GetAdaLocalTimeDifferenceParams = {
  ca: string,
  port: number,
};

export const getAdaLocalTimeDifference = (
  { ca, port }: GetAdaLocalTimeDifferenceParams
): Promise<AdaLocalTimeDifference> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/settings/time/difference',
    port,
    ca,
  })
);
