import { app, BrowserWindow, dialog, globalShortcut, Menu } from 'electron';
import { WalletSettingsStateEnum } from '../../common/ipc/api';
import { environment } from '../environment';
import { winLinuxMenu } from '../menus/win-linux';
import { osxMenu } from '../menus/osx';
import { logger } from './logging';
import { CardanoNode } from '../cardano/CardanoNode';
import { DIALOGS, PAGES } from '../../common/ipc/constants';
import { showUiPartChannel } from '../ipc/control-ui-parts';
import { getTranslation } from './getTranslation';

interface Data {
  isNavigationEnabled: boolean;
  walletSettingsState: WalletSettingsStateEnum;
}
export const buildAppMenus = async (
  mainWindow: BrowserWindow,
  cardanoNode: CardanoNode | null | undefined,
  locale: string,
  { isNavigationEnabled, walletSettingsState }: Data
) => {
  const {
    ABOUT,
    DAEDALUS_DIAGNOSTICS,
    ITN_REWARDS_REDEMPTION,
    TOGGLE_RTS_FLAGS_MODE,
  } = DIALOGS;
  const { SETTINGS, WALLET_SETTINGS } = PAGES;
  const { isMacOS, isBlankScreenFixActive } = environment;

  const translations = require(`../locales/${locale}`);

  const openAboutDialog = () => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
    if (mainWindow) showUiPartChannel.send(ABOUT, mainWindow);
  };

  const openDaedalusDiagnosticsDialog = () => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
    if (mainWindow) showUiPartChannel.send(DAEDALUS_DIAGNOSTICS, mainWindow);
  };

  const openItnRewardsRedemptionDialog = () => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
    if (mainWindow) showUiPartChannel.send(ITN_REWARDS_REDEMPTION, mainWindow);
  };

  const openSettingsPage = () => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
    if (mainWindow) showUiPartChannel.send(SETTINGS, mainWindow);
  };

  const openWalletSettingsPage = () => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
    if (mainWindow) showUiPartChannel.send(WALLET_SETTINGS, mainWindow);
  };

  const restartWithBlankScreenFix = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info('Restarting in BlankScreenFix...');
    if (cardanoNode) await cardanoNode.stop();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info('Exiting Daedalus with code 21', {
      code: 21,
    });
    // We have to make sure that cardano-node exits, otherwise we get DB locked errors at startup:
    (mainWindow as any).daedalusExitCode = 21;
    mainWindow.close();
  };

  const restartWithoutBlankScreenFix = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info('Restarting without BlankScreenFix...');
    if (cardanoNode) await cardanoNode.stop();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logger.info('Exiting Daedalus with code 22', {
      code: 22,
    });
    // We have to make sure that cardano-node exits, otherwise we get DB locked errors at startup:
    (mainWindow as any).daedalusExitCode = 22;
    mainWindow.close();
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

  const openToggleRTSFlagsModeDialog = () => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
    if (mainWindow) showUiPartChannel.send(TOGGLE_RTS_FLAGS_MODE, mainWindow);
  };

  const menuActions = {
    openAboutDialog,
    openDaedalusDiagnosticsDialog,
    openItnRewardsRedemptionDialog,
    openSettingsPage,
    openWalletSettingsPage,
    toggleBlankScreenFix,
    openToggleRTSFlagsModeDialog,
  };
  // Build app menus
  let menu;

  if (isMacOS) {
    menu = Menu.buildFromTemplate(
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ label: any; submenu: ({ label... Remove this comment to see the full error message
      osxMenu(
        app,
        mainWindow,
        menuActions,
        translations,
        locale,
        isNavigationEnabled,
        walletSettingsState
      )
    );
    Menu.setApplicationMenu(menu);
  } else {
    menu = Menu.buildFromTemplate(
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ label: any; submenu: ({ label... Remove this comment to see the full error message
      winLinuxMenu(
        app,
        mainWindow,
        menuActions,
        translations,
        locale,
        isNavigationEnabled,
        walletSettingsState
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
