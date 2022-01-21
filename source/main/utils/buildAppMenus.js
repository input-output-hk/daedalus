// @flow
import { app, BrowserWindow, dialog, globalShortcut, Menu } from 'electron';
import { environment } from '../environment';
import { winLinuxMenu } from '../menus/win-linux';
import { osxMenu } from '../menus/osx';
import { logger } from './logging';
import { safeExitWithCode } from './safeExitWithCode';
import { CardanoNode } from '../cardano/CardanoNode';
import { DIALOGS, PAGES } from '../../common/ipc/constants';
import { showUiPartChannel } from '../ipc/control-ui-parts';
import { getTranslation } from './getTranslation';
import { setRtsFlagsAndRestart } from './rtsFlags';
import { RTS_FLAGS } from '../config';

export const buildAppMenus = async (
  mainWindow: BrowserWindow,
  cardanoNode: ?CardanoNode,
  locale: string,
  data: {
    isNavigationEnabled: boolean,
  }
) => {
  const { ABOUT, DAEDALUS_DIAGNOSTICS, ITN_REWARDS_REDEMPTION } = DIALOGS;
  const { SETTINGS, WALLET_SETTINGS } = PAGES;
  const { isNavigationEnabled } = data;

  const { isMacOS, isBlankScreenFixActive } = environment;
  const translations = require(`../locales/${locale}`);

  const openAboutDialog = () => {
    if (mainWindow) showUiPartChannel.send(ABOUT, mainWindow);
  };

  const openDaedalusDiagnosticsDialog = () => {
    if (mainWindow) showUiPartChannel.send(DAEDALUS_DIAGNOSTICS, mainWindow);
  };

  const openItnRewardsRedemptionDialog = () => {
    if (mainWindow) showUiPartChannel.send(ITN_REWARDS_REDEMPTION, mainWindow);
  };

  const openSettingsPage = () => {
    if (mainWindow) showUiPartChannel.send(SETTINGS, mainWindow);
  };

  const openWalletSettingsPage = () => {
    if (mainWindow) showUiPartChannel.send(WALLET_SETTINGS, mainWindow);
  };

  const restartWithBlankScreenFix = async () => {
    logger.info('Restarting in BlankScreenFix...');
    if (cardanoNode) await cardanoNode.stop();
    logger.info('Exiting Daedalus with code 21', { code: 21 });
    safeExitWithCode(21);
  };

  const restartWithoutBlankScreenFix = async () => {
    logger.info('Restarting without BlankScreenFix...');
    if (cardanoNode) await cardanoNode.stop();
    logger.info('Exiting Daedalus with code 22', { code: 22 });
    safeExitWithCode(22);
  };

  const toggleBlankScreenFix = async (item) => {
    const translation = getTranslation(translations, 'menu');
    const blankScreenFixDialogOptions = {
      buttons: [
        translation('helpSupport.blankScreenFixDialogConfirm'),
        translation('helpSupport.blankScreenFixDialogCancel'),
      ],
      type: 'warning',
      title: isBlankScreenFixActive
        ? translation('helpSupport.blankScreenFixDialogTitle')
        : translation('helpSupport.nonBlankScreenFixDialogTitle'),
      message: isBlankScreenFixActive
        ? translation('helpSupport.blankScreenFixDialogMessage')
        : translation('helpSupport.nonBlankScreenFixDialogMessage'),
      defaultId: 1,
      cancelId: 1,
      noLink: true,
    };

    const { response } = await dialog.showMessageBox(
      mainWindow,
      blankScreenFixDialogOptions
    );
    if (response === 0) {
      if (isBlankScreenFixActive) {
        restartWithoutBlankScreenFix();
      } else {
        restartWithBlankScreenFix();
      }
    }
    item.checked = isBlankScreenFixActive;
  };

  const setRtsFlags = async (enable: boolean): Promise<void> => {
    const translation = getTranslation(translations, 'menu');
    const rtsFlagsDialogOptions = {
      buttons: [
        translation('helpSupport.rtsFlagsDialogConfirm'),
        translation('helpSupport.rtsFlagsDialogCancel'),
      ],
      type: 'warning',
      title: enable
        ? translation('helpSupport.enableRtsFlagsDialogTitle')
        : translation('helpSupport.disableRtsFlagsDialogTitle'),
      message: enable
        ? translation('helpSupport.enableRtsFlagsDialogMessage')
        : translation('helpSupport.disableRtsFlagsDialogMessage'),
      defaultId: 1,
      cancelId: 1,
      noLink: true,
    };

    const { response } = await dialog.showMessageBox(
      mainWindow,
      rtsFlagsDialogOptions
    );
    if (response === 0) {
      if (enable) {
        setRtsFlagsAndRestart(environment.network, RTS_FLAGS);
      } else {
        setRtsFlagsAndRestart(environment.network, []);
      }
    }
  };

  const menuActions = {
    openAboutDialog,
    openDaedalusDiagnosticsDialog,
    openItnRewardsRedemptionDialog,
    openSettingsPage,
    openWalletSettingsPage,
    toggleBlankScreenFix,
    setRtsFlags,
  };

  // Build app menus
  let menu;
  if (isMacOS) {
    menu = Menu.buildFromTemplate(
      osxMenu(
        app,
        mainWindow,
        menuActions,
        translations,
        locale,
        isNavigationEnabled
      )
    );
    Menu.setApplicationMenu(menu);
  } else {
    menu = Menu.buildFromTemplate(
      winLinuxMenu(
        app,
        mainWindow,
        menuActions,
        translations,
        locale,
        isNavigationEnabled
      )
    );
    mainWindow.setMenu(menu);
  }

  // Hide application window on Cmd+H hotkey (OSX only!)
  if (isMacOS) {
    app.on('activate', () => {
      if (!mainWindow.isVisible()) app.show();
    });

    mainWindow.on('focus', () => {
      globalShortcut.register('CommandOrControl+H', app.hide);
    });

    mainWindow.on('blur', () => {
      globalShortcut.unregister('CommandOrControl+H');
    });
  }
};
