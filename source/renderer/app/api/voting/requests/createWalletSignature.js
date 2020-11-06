// @flow
import type { RequestConfig } from '../../common/types';
import type { SignatureParams } from '../types';
import { request } from '../../utils/request';
import { getContentLength } from '../../utils';

export const createWalletSignature = (
  config: RequestConfig,
  { walletId, role, index, data }: SignatureParams
): Promise<string> =>
  request(
    {
      method: 'POST',
      path: `/v2/wallets/${walletId}/signatures/${role}/${index}`,
      headers: {
        'Content-Length': getContentLength(JSON.stringify(data)),
        'Content-Type': 'application/json; charset=utf-8',
        Accept: 'application/octet-stream',
      },
      ...config,
    },
    {},
    data
  );
