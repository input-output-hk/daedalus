// @flow
import { app, globalShortcut, Menu, BrowserWindow, dialog } from 'electron';
import { get } from 'lodash';
import { environment } from '../environment';
import { winLinuxMenu } from '../menus/win-linux';
import { osxMenu } from '../menus/osx';
import { Logger } from './logging';
import { safeExitWithCode } from './safeExitWithCode';
import { CardanoNode } from '../cardano/CardanoNode';
import { DIALOGS, SCREENS } from '../../common/ipc/constants';
import { showUiPartChannel } from '../ipc/control-ui-parts';
import { getLocale } from './getLocale';
import { getTranslation } from './getTranslation';

const localesFillForm = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};

export const buildAppMenus = async (
  mainWindow: BrowserWindow,
  cardanoNode: ?CardanoNode,
  locale: string
) => {
  const { ADA_REDEMPTION } = SCREENS;
  const { ABOUT, BLOCK_CONSOLIDATION, DAEDALUS_DIAGNOSTICS } = DIALOGS;

  const {
    isMacOS,
    version,
    apiVersion,
    network,
    build,
    installerVersion,
    os,
    buildNumber,
    isInSafeMode,
  } = environment;
  const translations = require(`../locales/${locale}`);
  const networkLocale = getLocale(network);

  const openAboutDialog = () => {
    if (mainWindow) showUiPartChannel.send(ABOUT, mainWindow);
  };

  const openAdaRedemptionScreen = () => {
    if (mainWindow) showUiPartChannel.send(ADA_REDEMPTION, mainWindow);
  };

  const openBlockConsolidationStatusDialog = () => {
    if (mainWindow) showUiPartChannel.send(BLOCK_CONSOLIDATION, mainWindow);
  };

  const openDaedalusDiagnosticsDialog = () => {
    if (mainWindow) showUiPartChannel.send(DAEDALUS_DIAGNOSTICS, mainWindow);
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

  const toggleOnSafeMode = item => {
    const translation = getTranslation(translations, 'menu');
    const gpuSafeModeDialogOptions = {
      buttons: [
        translation('helpSupport.gpuSafeModeDialogConfirm'),
        translation('helpSupport.gpuSafeModeDialogCancel'),
      ],
      type: 'warning',
      title: isInSafeMode
        ? translation('helpSupport.gpuSafeModeDialogTitle')
        : translation('helpSupport.nonGpuSafeModeDialogTitle'),
      message: isInSafeMode
        ? translation('helpSupport.gpuSafeModeDialogMessage')
        : translation('helpSupport.nonGpuSafeModeDialogMessage'),
      defaultId: 1,
      cancelId: 1,
      noLink: true,
    };
    dialog.showMessageBox(mainWindow, gpuSafeModeDialogOptions, buttonId => {
      if (buttonId === 0) {
        if (isInSafeMode) {
          restartWithoutSafeMode();
        } else {
          restartInSafeMode();
        }
      }
      item.checked = isInSafeMode;
    });
  };

  const supportRequestData = {
    frontendVersion: version,
    backendVersion: apiVersion,
    network: network === 'development' ? 'staging' : network,
    build,
    installerVersion,
    os,
    networkLocale,
    product: `Daedalus wallet - ${network}`,
    supportLanguage: localesFillForm[networkLocale],
    productVersion: `Daedalus ${version}+Cardano ${buildNumber}`,
  };

  const menuActions = {
    openAboutDialog,
    openDaedalusDiagnosticsDialog,
    openAdaRedemptionScreen,
    toggleOnSafeMode,
    openBlockConsolidationStatusDialog,
  };

  // Build app menus
  let menu;
  const isNodeInSync = get(cardanoNode, 'status.isNodeInSync', false);
  if (isMacOS) {
    menu = Menu.buildFromTemplate(
      osxMenu(
        app,
        mainWindow,
        menuActions,
        translations,
        supportRequestData,
        isNodeInSync
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
        supportRequestData,
        isNodeInSync
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
