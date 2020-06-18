// @flow

export type IntrospectAddressRequest = {
  input: string
};

export type AddressStyle = 'Byron' | 'Icarus' | 'Jormungandr' | 'Shelley'

export type ChainPointer = {
  slot_num: number,
  transaction_index: number,
  output_index: number
}

export type AddressBase = {
  address_style: AddressStyle,
  network_tag: number | null
}

export type ByronAddress = AddressBase & {
  stake_reference: 'none',
  address_root: string,
  derivation_path: string,
}

export type IcarusAddress = AddressBase & {
  stake_reference: 'none',
  address_root: string,
}

export type JormungandrAddress = AddressBase & {
  address_type: 'single' |'group' | 'account' | 'multisig',
  account_key?: string,
  merkle_root?: string,
  stake_reference: 'by value' | 'none',
  spending_key?: string,
  stake_key?: string,
}

export type ShelleyAddress = AddressBase & {
  pointer?: ChainPointer,
  stake_reference: 'by pointer' | 'none',
  script_hash?: string,
  spending_key_hash?: string,
  stake_key_hash?: string,
  stake_script_hash?: string,
}

export type IntrospectAddressResponse = {
  introspection: ByronAddress | IcarusAddress | JormungandrAddress | ShelleyAddress
} | 'Invalid';
