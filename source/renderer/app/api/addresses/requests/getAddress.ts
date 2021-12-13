import type { RequestConfig } from '../../common/types';
import type { Address } from '../types';
import { request } from '../../utils/request';

export type GetAddressParams = {
  address: string;
};
export const getAddress = (
  config: RequestConfig,
  { address }: GetAddressParams
): Promise<Address> => {
  const encodedAddress = encodeURIComponent(address);
  return request({
    method: 'GET',
    path: `/api/v1/addresses/${encodedAddress}`,
    ...config,
  });
};
