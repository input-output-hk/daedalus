import type { RequestConfig } from '../../common/types';
import type { ApiDRepSummary } from '../types';
import { request } from '../../utils/request';

export const getDRepSummary = (
  config: RequestConfig
): Promise<ApiDRepSummary> =>
  request({ method: 'GET', path: '/v2/dreps/summary', ...config });
