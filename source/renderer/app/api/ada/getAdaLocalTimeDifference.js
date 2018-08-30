// @flow
import type { AdaLocalTimeDifference, RequestConfig } from './types';
import { request } from './lib/request';

export const getAdaLocalTimeDifference = (
  config: RequestConfig
): Promise<AdaLocalTimeDifference> => (
  request(Object.assign({
    hostname: 'localhost',
    method: 'GET',
    path: '/api/settings/time/difference',
  }, config))
);
