// @flow
export type Address = {
  id: string,
  used: boolean,
  changeAddress: boolean,
};

export type Addresses = Array<Address>;

// req/res Address types
export type GetAddressesResponse = {
  accountIndex: ?number,
  addresses: Addresses,
};

export type GetAddressesRequest = {
  walletId: string,
};

export type CreateAddressRequest = {
  spendingPassword: ?string,
  accountIndex: number,
  walletId: string,
};
