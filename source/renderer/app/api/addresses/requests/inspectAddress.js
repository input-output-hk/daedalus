// @flow
import { request } from '../../utils/request';
import type { RequestConfig } from '../../common/types';

export const inspectAddress = (
  config: RequestConfig,
  { addressId }: any // @TODO
): Promise<any> =>
  request(
    {
      method: 'GET',
      path: `/v2/addresses/${addressId}`,
      ...config,
    },
    {},
    {}
  );
