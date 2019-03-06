// @flow
import { app, globalShortcut, Menu, BrowserWindow } from 'electron';
import { environment } from '../environment';
import { winLinuxMenu } from '../menus/win-linux';
import { osxMenu } from '../menus/osx';
import { Logger } from './logging';
import { safeExitWithCode } from './safeExitWithCode';
import { CardanoNode } from '../cardano/CardanoNode';
import { DIALOGS, SCREENS } from '../../common/ipc/constants';
import {
  toggleUiPartChannel,
  showUiPartChannel,
} from '../ipc/control-ui-parts';

export const buildAppMenus = async (
  mainWindow: BrowserWindow,
  cardanoNode: ?CardanoNode,
  isInSafeMode: boolean,
  locale: string
) => {
  const openAbout = () => {
    if (mainWindow) toggleUiPartChannel.send(DIALOGS.ABOUT, mainWindow);
  };

  const openNetworkStatus = () => {
    if (mainWindow)
      toggleUiPartChannel.send(DIALOGS.NETWORK_STATUS, mainWindow);
  };

  const goToAdaRedemption = () => {
    if (mainWindow) showUiPartChannel.send(SCREENS.ADA_REDEMPTION, mainWindow);
  };

  const goBlockConsolidationStatus = () => {
    if (mainWindow)
      toggleUiPartChannel.send(SCREENS.BLOCK_CONSOLIDATION, mainWindow);
  };

  const restartInSafeMode = async () => {
    Logger.info('Restarting in SafeMode...');
    if (cardanoNode) await cardanoNode.stop();
    Logger.info('Exiting Daedalus with code 21', { code: 21 });
    safeExitWithCode(21);
  };

  const restartWithoutSafeMode = async () => {
    Logger.info('Restarting without SafeMode...');
    if (cardanoNode) await cardanoNode.stop();
    Logger.info('Exiting Daedalus with code 22', { code: 22 });
    safeExitWithCode(22);
  };

  const { isMacOS } = environment;
  const translations = require(`../locales/${locale}`);
  const menuActions = {
    openAbout,
    openNetworkStatus,
    goToAdaRedemption,
    restartInSafeMode,
    restartWithoutSafeMode,
    goBlockConsolidationStatus,
  };

  // Build app menus
  let menu;
  if (isMacOS) {
    menu = Menu.buildFromTemplate(
      osxMenu(app, mainWindow, menuActions, isInSafeMode, translations)
    );
    Menu.setApplicationMenu(menu);
  } else {
    menu = Menu.buildFromTemplate(
      winLinuxMenu(app, mainWindow, menuActions, isInSafeMode, translations)
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
