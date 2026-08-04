import type { RequestConfig } from '../../common/types';
import type { ApiDRepInfo } from '../types';
import { request } from '../../utils/request';

export const listSuggestedDReps = (
  count: number,
  config: RequestConfig
): Promise<ApiDRepInfo[]> =>
  request({ method: 'GET', path: `/v2/dreps/suggested?count=${count}`, ...config });
