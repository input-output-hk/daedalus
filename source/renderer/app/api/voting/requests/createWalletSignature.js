// @flow
import type { RequestConfig } from '../../common/types';
import type { SignatureParams } from '../types';
import { request } from '../../utils/requestBinary';

export const createWalletSignature = (
  config: RequestConfig,
  { walletId, role, index, data }: SignatureParams
): Promise<Buffer> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/signatures/${role}/${index}`,
      ...config,
    },
    {},
    data
  );
