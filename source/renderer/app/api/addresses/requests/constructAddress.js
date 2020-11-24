// @flow
import { request } from '../../utils/request';

export const constructAddress = (
  config: RequestConfig,
  { data }: any // @TODO
): Promise<any> =>
  request(
    {
      method: 'POST',
      path: '/v2/addresses',
      ...config,
    },
    {},
    data
  );
