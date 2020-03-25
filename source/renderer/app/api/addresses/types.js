// @flow
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
  isLegacy: boolean,
  queryParams?: GetAddressesRequestQueryParams,
};

// Byron related types

export type CreateByronWalletAddressRequest = {
  walletId: string,
  passphrase: string,
  addressIndex?: number,
};
