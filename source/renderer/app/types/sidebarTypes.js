// @flow
export type SidebarWalletType = {
  id: string,
  title: string,
  info: string,
  isConnected: boolean,
  isRestoreActive?: boolean,
  restoreProgress?: number,
  isLegacy: boolean,
  recoveryPhraseVerificationStatus: string,
};
