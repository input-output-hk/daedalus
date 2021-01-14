// @flow
import { observable } from 'mobx';
import type { AddressStyle } from '../api/addresses/types';

type WalletAddressProps = {
  id: string,
  used: boolean,
  spendingPath: string,
};

export const AddressStyles: {
  ADDRESS_BYRON: AddressStyle,
  ADDRESS_SHELLEY: AddressStyle,
  ADDRESS_ICARUS: AddressStyle,
} = {
  ADDRESS_BYRON: 'Byron',
  ADDRESS_SHELLEY: 'Shelley',
  ADDRESS_ICARUS: 'Icarus',
};

export default class WalletAddress {
  @observable id: string = '';
  @observable used: boolean = false;
  @observable spendingPath: string = "1852'/1815'/0'";

  constructor(data: WalletAddressProps) {
    Object.assign(this, data);
  }
}
