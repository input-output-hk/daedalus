import BigNumber from 'bignumber.js';

export type SidebarWalletType = {
  id: string;
  title: string;
  amount: BigNumber;
  isConnected: boolean;
  isRestoreActive?: boolean;
  restoreProgress?: number;
  isLegacy: boolean;
  isNotResponding: boolean;
  hasNotification: boolean;
};
export type WalletSortByOptions = 'DATE' | 'BALANCE' | 'NAME' | 'NONE';
export type WalletSortOrderOptions = 'asc' | 'desc';
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
export const WalletSortBy: EnumMap<string, WalletSortByOptions> = {
  Date: 'DATE',
  Balance: 'BALANCE',
  Name: 'NAME',
  None: 'NONE',
};
// @ts-ignore ts-migrate(2304) FIXME: Cannot find name 'EnumMap'.
export const WalletSortOrder: EnumMap<
  string,
  WalletSortOrderOptions
> = Object.freeze({
  Desc: 'desc',
  Asc: 'asc',
});
export type WalletSortConfig = {
  sortBy: WalletSortByOptions;
  sortOrder: WalletSortOrderOptions;
};
