// @flow
import type { RequestConfig } from '../../common/types';
import type { NodeSettingsResponse } from '../types';
import { request } from '../../utils/request';

export const getNodeSettings = (
  config: RequestConfig
): Promise<NodeSettingsResponse> =>
  request({
    method: 'GET',
    path: '/api/v1/node-settings',
    ...config,
  });
