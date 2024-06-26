import { ApplicationDialog } from '../../renderer/app/types/applicationDialogTypes';

export const DIALOGS: Record<string, ApplicationDialog> = {
  ABOUT: 'ABOUT_DIALOG',
  DAEDALUS_DIAGNOSTICS: 'DAEDALUS_DIAGNOSTICS_DIALOG',
  ITN_REWARDS_REDEMPTION: 'ITN_REWARDS_REDEMPTION_DIALOG',
  TOGGLE_RTS_FLAGS_MODE: 'TOGGLE_RTS_FLAGS_MODE_DIALOG',
};
export const NOTIFICATIONS = {
  DOWNLOAD_LOGS: 'DOWNLOAD_LOGS_NOTIFICATION',
};
export const PAGES = {
  SETTINGS: 'SETTINGS',
  WALLET_SETTINGS: 'WALLET_SETTINGS',
};
