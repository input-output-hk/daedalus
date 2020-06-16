// @flow
export type AddressStyle = 'Byron' | 'Icarus' | 'Jormungandr' | 'Shelley';

export type IntrospectAddressRequest = {
  input: string
};

export type IntrospectAddressResponse = {
  addressStyle: AddressStyle,
  stakeReference?: string,
  spendingKeyHash?: string,
  stakeKeyHash?: string,
  networkTag?: string
} | false;
