import { observable, makeObservable } from 'mobx';
import type { AddressStyle } from '../api/addresses/types';

type WalletAddressProps = {
  id: string;
  used: boolean;
  spendingPath: string;
};
export const AddressStyles: {
  ADDRESS_BYRON: AddressStyle;
  ADDRESS_SHELLEY: AddressStyle;
  ADDRESS_ICARUS: AddressStyle;
} = {
  ADDRESS_BYRON: 'Byron',
  ADDRESS_SHELLEY: 'Shelley',
  ADDRESS_ICARUS: 'Icarus',
};
export default class WalletAddress {
  id = '';
  used = false;
  spendingPath = "1852'/1815'/0'";

  constructor(data: WalletAddressProps) {
    makeObservable(this, {
      id: observable,
      used: observable,
      spendingPath: observable,
    });

    Object.assign(this, data);
  }
}
