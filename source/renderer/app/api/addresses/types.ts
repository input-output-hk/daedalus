export type AddressState = 'used' | 'unused';
export type AddressStyle = 'Byron' | 'Shelley' | 'Icarus';
export type StakeReference = 'none' | 'by value' | 'by pointer';
export type GetAddressesRequestQueryParams = {
  state: AddressState;
};
export type Address = {
  id: string;
  state: AddressState;
  derivation_path: Array<string>;
};
export type Addresses = Array<Address>;
export type GetAddressesRequest = {
  walletId: string;
  isLegacy: boolean;
  queryParams?: GetAddressesRequestQueryParams;
  isHardwareWallet?: boolean;
};
export type InspectAddressResponse = {
  address_style: AddressStyle;
  stake_reference: StakeReference;
  network_tag?: number;
  spending_key_hash?: string;
  stake_key_hash?: string;
  script_hash?: string;
  pointer?: {
    slot_num: number;
    transaction_index: number;
    output_index: number;
  };
  address_root?: string;
  derivation_path?: string;
};
// Byron related types
export type CreateByronWalletAddressRequest = {
  walletId: string;
  passphrase: string;
  addressIndex?: number;
};
