import { request } from '../../utils/request';
import type { RequestConfig } from '../../common/types';

export const getPublicKey = (
  config: RequestConfig,
  { walletId, role, index }: any // @TODO
): Promise<any> =>
  request(
    {
      method: 'GET',
      path: `/v2/wallets/${walletId}/keys/${role}/${index}`,
      ...config,
    },
    {},
    {}
  );
