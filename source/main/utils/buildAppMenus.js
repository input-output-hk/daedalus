// @flow
import { app, globalShortcut, Menu, BrowserWindow } from 'electron';
import { environment } from '../environment';
import { winLinuxMenu } from '../menus/win-linux';
import { osxMenu } from '../menus/osx';
import { Logger } from './logging';
import { safeExitWithCode } from './safeExitWithCode';
import { CardanoNode } from '../cardano/CardanoNode';
import {
  OPEN_ABOUT_DIALOG_CHANNEL,
  GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL,
  GO_TO_NETWORK_STATUS_SCREEN_CHANNEL
} from '../../common/ipc/api';

export const buildAppMenus = async (
  mainWindow: BrowserWindow,
  cardanoNode: ?CardanoNode,
  isInSafeMode: boolean,
  locale: string,
) => {

  const openAbout = () => {
    if (mainWindow) mainWindow.webContents.send(OPEN_ABOUT_DIALOG_CHANNEL);
  };

  const goToAdaRedemption = () => {
    if (mainWindow) mainWindow.webContents.send(GO_TO_ADA_REDEMPTION_SCREEN_CHANNEL);
  };

  const goToNetworkStatus = () => {
    if (mainWindow) mainWindow.webContents.send(GO_TO_NETWORK_STATUS_SCREEN_CHANNEL);
  };

  const restartInSafeMode = async () => {
    Logger.info('Restarting in SafeMode...');
    if (cardanoNode) await cardanoNode.stop();
    Logger.info('Exiting Daedalus with code 21.');
    safeExitWithCode(21);
  };

  const restartWithoutSafeMode = async () => {
    Logger.info('Restarting without SafeMode...');
    if (cardanoNode) await cardanoNode.stop();
    Logger.info('Exiting Daedalus with code 22.');
    safeExitWithCode(22);
  };

  const { isMacOS } = environment;
  const translations = require(`../locales/${locale}`);
  const menuActions = {
    openAbout,
    goToAdaRedemption,
    goToNetworkStatus,
    restartInSafeMode,
    restartWithoutSafeMode,
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
      winLinuxMenu(app, mainWindow, menuActions, isInSafeMode)
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
