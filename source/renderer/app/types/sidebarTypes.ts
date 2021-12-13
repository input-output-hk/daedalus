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
