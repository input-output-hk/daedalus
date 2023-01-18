export type IntrospectAddressRequest = {
  input: string;
};
export type AddressStyle = 'Byron' | 'Icarus' | 'Shelley';
export type AddressType = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 14 | 15;
export type ChainPointer = {
  slot_num: number;
  transaction_index: number;
  output_index: number;
};
export type AddressBase = {
  address_type: AddressType;
  address_style: AddressStyle;
  network_tag: number | null;
  stake_reference: 'none' | 'by pointer' | 'by value';
};
export type ByronAddress = AddressBase & {
  address_root: string;
  derivation_path: string;
};
export type IcarusAddress = AddressBase & {
  address_root: string;
};
export type ShelleyAddress = AddressBase & {
  pointer?: ChainPointer;
  script_hash?: string;
  spending_key_hash?: string;
  stake_key_hash?: string;
  stake_script_hash?: string;
};
export type IntrospectAddressResponse =
  | {
      introspection: ByronAddress | IcarusAddress | ShelleyAddress;
    }
  | 'Invalid';
