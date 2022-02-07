import { request } from '../../utils/request';
import type { RequestConfig } from '../../common/types';
import type { InspectAddressResponse } from '../types';

export const inspectAddress = (
  config: RequestConfig,
  { addressId }: any // @TODO
): Promise<InspectAddressResponse> =>
  request(
    {
      method: 'GET',
      path: `/v2/addresses/${addressId}`,
      ...config,
    },
    {},
    {}
  );
