import type { RequestConfig } from '../../common/types';
import type { PoolMetadataSource } from '../types';
import { request } from '../../utils/request';

export const updateSmashSettings = (
  config: RequestConfig,
  poolMetadataSource: PoolMetadataSource
): Promise<any> =>
  request(
    {
      method: 'PUT',
      path: '/v2/settings',
      ...config,
    },
    {},
    {
      settings: {
        pool_metadata_source: poolMetadataSource,
      },
    }
  );
