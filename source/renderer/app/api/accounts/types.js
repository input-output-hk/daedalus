// @flow
import type { AdaAddresses } from '../addresses/types';

export type Account = {
  amount: number,
  addresses: AdaAddresses,
  name: string,
  walletId: string,
  index: number,
};

export type Accounts = Array<Account>;
