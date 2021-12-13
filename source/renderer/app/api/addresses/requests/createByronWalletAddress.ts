// @flow
import type { RequestConfig } from '../../common/types';
import type { Address } from '../types';
import { request } from '../../utils/request';
import { getRawWalletId } from '../../utils';

export type CreateAddressParams = {
  walletId: string,
  passphrase: string,
  addressIndex?: number,
};

export const createByronWalletAddress = (
  config: RequestConfig,
  { passphrase, addressIndex, walletId }: CreateAddressParams
): Promise<Address> => {
  let data = { passphrase };
  data = addressIndex ? { ...data, address_index: addressIndex } : data;
  return request(
    {
      method: 'POST',
      path: `/v2/byron-wallets/${getRawWalletId(walletId)}/addresses`,
      ...config,
    },
    {},
    data
  );
};
