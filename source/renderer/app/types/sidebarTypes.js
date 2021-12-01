// @flow
import BigNumber from 'bignumber.js';

export type SidebarWalletType = {
  id: string,
  title: string,
  amount: BigNumber,
  isConnected: boolean,
  isRestoreActive?: boolean,
  restoreProgress?: number,
  isLegacy: boolean,
  isNotResponding: boolean,
  hasNotification: boolean,
};

export type WalletSortByOptions = 'DATE' | 'BALANCE' | 'NAME' | 'NONE';

export type WalletSortOrderOptions = 'asc' | 'desc';

export const WalletSortBy: EnumMap<string, WalletSortByOptions> = Object.freeze(
  {
    Date: 'DATE',
    Balance: 'BALANCE',
    Name: 'NAME',
    None: 'NONE',
  }
);

export const WalletSortOrder: EnumMap<
  string,
  WalletSortOrderOptions
> = Object.freeze({
  Desc: 'desc',
  Asc: 'asc',
});
