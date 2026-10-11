import { dialog, shell } from 'electron';
import type { BrowserWindow, MessageBoxSyncOptions } from 'electron';
import { getTranslation } from './getTranslation';

// Tells the user that the update installer could not be started. By then the
// backend has stopped for the update and Daedalus is about to close, so the
// dialog says so and offers to show the installer, which can be run by hand.
export const showInstallerLaunchFailure = (
  window: BrowserWindow | null,
  locale: string,
  installerPath: string,
  reason: string
): void => {
  const translations = require(`../locales/${locale}`);
  const t = getTranslation(translations, 'dialog.updateInstallerFailed');
  const options: MessageBoxSyncOptions = {
    type: 'error',
    title: t('title'),
    message: t('message'),
    detail: `${t('detail')}\n${installerPath}\n\n${reason}`,
    buttons: [t('showInstaller'), t('close')],
    defaultId: 0,
    cancelId: 1,
    noLink: true,
  };
  const response =
    window && !window.isDestroyed()
      ? dialog.showMessageBoxSync(window, options)
      : dialog.showMessageBoxSync(options);
  if (response === 0) {
    shell.showItemInFolder(installerPath);
  }
};
