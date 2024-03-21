'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.buildAppMenus = void 0;
const electron_1 = require('electron');
const environment_1 = require('../environment');
const win_linux_1 = require('../menus/win-linux');
const osx_1 = require('../menus/osx');
const logging_1 = require('./logging');
const safeExitWithCode_1 = require('./safeExitWithCode');
const constants_1 = require('../../common/ipc/constants');
const control_ui_parts_1 = require('../ipc/control-ui-parts');
const getTranslation_1 = require('./getTranslation');
const buildAppMenus = async (
  mainWindow,
  cardanoNode,
  locale,
  { isNavigationEnabled, walletSettingsState }
) => {
  const {
    ABOUT,
    DAEDALUS_DIAGNOSTICS,
    ITN_REWARDS_REDEMPTION,
    TOGGLE_RTS_FLAGS_MODE,
  } = constants_1.DIALOGS;
  const { SETTINGS, WALLET_SETTINGS } = constants_1.PAGES;
  const { isMacOS, isBlankScreenFixActive } = environment_1.environment;
  const translations = require(`../locales/${locale}`);
  const openAboutDialog = () => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
    if (mainWindow)
      control_ui_parts_1.showUiPartChannel.send(ABOUT, mainWindow);
  };
  const openDaedalusDiagnosticsDialog = () => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
    if (mainWindow)
      control_ui_parts_1.showUiPartChannel.send(
        DAEDALUS_DIAGNOSTICS,
        mainWindow
      );
  };
  const openItnRewardsRedemptionDialog = () => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
    if (mainWindow)
      control_ui_parts_1.showUiPartChannel.send(
        ITN_REWARDS_REDEMPTION,
        mainWindow
      );
  };
  const openSettingsPage = () => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
    if (mainWindow)
      control_ui_parts_1.showUiPartChannel.send(SETTINGS, mainWindow);
  };
  const openWalletSettingsPage = () => {
    // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
    if (mainWindow)
      control_ui_parts_1.showUiPartChannel.send(WALLET_SETTINGS, mainWindow);
  };
  const restartWithBlankScreenFix = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('Restarting in BlankScreenFix...');
    if (cardanoNode) await cardanoNode.stop();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('Exiting Daedalus with code 21', {
      code: 21,
    });
    (0, safeExitWithCode_1.safeExitWithCode)(21);
  };
  const restartWithoutBlankScreenFix = async () => {
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('Restarting without BlankScreenFix...');
    if (cardanoNode) await cardanoNode.stop();
    // @ts-ignore ts-migrate(2554) FIXME: Expected 2 arguments, but got 1.
    logging_1.logger.info('Exiting Daedalus with code 22', {
      code: 22,
    });
    (0, safeExitWithCode_1.safeExitWithCode)(22);
  };
  const toggleBlankScreenFix = async (item) => {
    const translation = (0, getTranslation_1.getTranslation)(
      translations,
      'menu'
    );
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
    const { response } = await electron_1.dialog.showMessageBox(
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
    if (mainWindow)
      control_ui_parts_1.showUiPartChannel.send(
        TOGGLE_RTS_FLAGS_MODE,
        mainWindow
      );
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
    menu = electron_1.Menu.buildFromTemplate(
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ label: any; submenu: ({ label... Remove this comment to see the full error message
      (0, osx_1.osxMenu)(
        electron_1.app,
        mainWindow,
        menuActions,
        translations,
        locale,
        isNavigationEnabled,
        walletSettingsState
      )
    );
    electron_1.Menu.setApplicationMenu(menu);
  } else {
    menu = electron_1.Menu.buildFromTemplate(
      // @ts-ignore ts-migrate(2345) FIXME: Argument of type '({ label: any; submenu: ({ label... Remove this comment to see the full error message
      (0, win_linux_1.winLinuxMenu)(
        electron_1.app,
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
    electron_1.app.on('activate', () => {
      if (!mainWindow.isVisible()) electron_1.app.show();
    });
    mainWindow.on('focus', () => {
      electron_1.globalShortcut.register(
        'CommandOrControl+H',
        electron_1.app.hide
      );
    });
    mainWindow.on('blur', () => {
      electron_1.globalShortcut.unregister('CommandOrControl+H');
    });
  }
};
exports.buildAppMenus = buildAppMenus;
//# sourceMappingURL=buildAppMenus.js.map
