import type { RequestConfig } from '../../common/types';
import type { ApiDRepInfo } from '../types';
import { request } from '../../utils/request';

export const getDRep = (
  drepId: string,
  config: RequestConfig
): Promise<ApiDRepInfo> =>
  request({ method: 'GET', path: `/v2/dreps/${drepId}`, ...config });
