import type { RequestConfig } from '../../common/types';
import type { AdaWallet } from '../types';
import { request } from '../../utils/request';

export const updateWallet = (
  config: RequestConfig,
  {
    walletId,
    name,
    singleAddressMode,
  }: {
    walletId: string;
    name?: string;
    singleAddressMode?: boolean;
  }
): Promise<AdaWallet> => {
  if (!global.cip30Enabled && singleAddressMode !== undefined)
    return Promise.reject(
      new Error('Single-address mode is unavailable for this package flavor')
    );
  return request(
    {
      method: 'PUT',
      path: `/v2/wallets/${walletId}`,
      ...config,
    },
    {},
    {
      ...(name === undefined ? {} : { name }),
      ...(singleAddressMode === undefined
        ? {}
        : { single_address_mode: singleAddressMode }),
    }
  );
};
