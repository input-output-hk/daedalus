// @flow
import WalletAddress from '../../domains/WalletAddress';

export type Address = {
  id: string,
  state: 'used' | 'unused',
};

export type Addresses = Array<Address>;

export type GetAddressesResponse = {
  accountIndex: ?number,
  addresses: Array<WalletAddress>,
};

export type GetAddressesRequest = {
  walletId: string,
};

export type CreateAddressRequest = {
  spendingPassword: ?string,
  accountIndex: number,
  walletId: string,
};
