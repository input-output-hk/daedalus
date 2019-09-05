// @flow
import WalletAddress from '../../domains/WalletAddress';

export type AddressState = 'used' | 'unused';

export type GetAddressesRequestQueryParams = {
  state: AddressState,
};

export type Address = {
  id: string,
  state: AddressState,
};

export type Addresses = Array<Address>;

export type GetAddressesRequest = {
  walletId: string,
  queryParams?: GetAddressesRequestQueryParams,
};

export type GetAddressesResponse = {
  accountIndex: ?number,
  addresses: Array<WalletAddress>,
};

export type CreateAddressRequest = {
  spendingPassword: ?string,
  accountIndex: number,
  walletId: string,
};
