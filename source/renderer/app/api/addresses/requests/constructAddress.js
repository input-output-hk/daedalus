// @flow
import { request } from '../../utils/request';

export const constructAddress = (
  config: RequestConfig,
  { publicKey }: any // @TODO
): Promise<any> =>
  request(
    {
      method: 'POST',
      path: '/v2/addresses',
      ...config,
    },
    {},
    {
      stake: publicKey,
    }
  );
