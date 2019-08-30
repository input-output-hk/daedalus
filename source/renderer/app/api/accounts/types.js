// @flow
import type { Addresses } from '../addresses/types';

export type Account = {
  amount: number,
  addresses: Addresses,
  name: string,
  walletId: string,
  index: number,
};

export type Accounts = Array<Account>;
