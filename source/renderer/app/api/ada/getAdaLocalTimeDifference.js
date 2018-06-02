// @flow
import type { AdaLocalTimeDifference } from './types';
import { request } from './lib/request';

export type GetAdaLocalTimeDifferenceParams = {
};

export const getAdaLocalTimeDifference = (
): Promise<AdaLocalTimeDifference> => (
  request({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/settings/time/difference',
  })
);
