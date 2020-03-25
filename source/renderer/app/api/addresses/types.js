// @flow
export type AddressState = 'used' | 'unused';

export type GetAddressesRequestQueryParams = {
  state: AddressState,
};

export type Address = {
  id: string,
  state: AddressState,
  accountIndex: number,
};

export type Addresses = Array<Address>;

export type GetAddressesRequest = {
  walletId: string,
  isLegacy: boolean,
  queryParams?: GetAddressesRequestQueryParams,
};

// Byron related types
export type ByronWalletAddress = {
  id: string,
  used: boolean,
  changeAddress: boolean,
};

export type ByronWalletAddresses = Array<ByronWalletAddress>;

export type CreateByronWalletAddressRequest = {
  spendingPassword: ?string,
  accountIndex: number,
  walletId: string,
};
