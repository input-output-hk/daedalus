'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.osxMenu = void 0;
const lodash_1 = require('lodash');
const electron_1 = require('electron');
const getTranslation_1 = require('../utils/getTranslation');
const environment_1 = require('../environment');
const control_ui_parts_1 = require('../ipc/control-ui-parts');
const constants_1 = require('../../common/ipc/constants');
const reporting_1 = require('../../common/utils/reporting');
const submenuBuilders_1 = require('./submenuBuilders');
const api_1 = require('../../common/ipc/api');
const id = 'menu';
const osxMenu = (
  app,
  window,
  actions,
  translations,
  locale,
  isNavigationEnabled,
  walletSettingsState,
  translation = (0, getTranslation_1.getTranslation)(translations, id)
) => [
  {
    label: translation('daedalus'),
    submenu: (0, lodash_1.compact)([
      {
        label: translation('daedalus.about'),
        click() {
          actions.openAboutDialog();
        },
        enabled: isNavigationEnabled,
      },
      {
        type: 'separator',
      },
      {
        label: translation('daedalus.redeemItnRewards'),
        accelerator: 'Command+T',
        click() {
          actions.openItnRewardsRedemptionDialog();
        },
        enabled: isNavigationEnabled,
      },
      {
        type: 'separator',
      },
      {
        label: translation('daedalus.settings'),
        accelerator: 'Command+,',
        click() {
          actions.openSettingsPage();
        },
        enabled: isNavigationEnabled,
      },
      {
        label: translation('daedalus.walletSettings'),
        accelerator: 'Command+;',
        click() {
          actions.openWalletSettingsPage();
        },
        enabled:
          isNavigationEnabled &&
          walletSettingsState === api_1.WalletSettingsStateEnum.enabled,
        visible: walletSettingsState !== api_1.WalletSettingsStateEnum.hidden,
      },
      {
        type: 'separator',
      },
      {
        label: translation('daedalus.hideDaedalus'),
        role: 'hide',
      },
      {
        label: translation('daedalus.hideOthers'),
        role: 'hideothers',
      },
      {
        label: translation('daedalus.showAll'),
        role: 'unhide',
      },
      {
        type: 'separator',
      },
      {
        label: translation('daedalus.quit'),
        accelerator: 'Command+Q',
        click() {
          app.quit();
        },
      },
    ]),
  },
  {
    label: translation('edit'),
    submenu: [
      {
        label: translation('edit.undo'),
        accelerator: 'Command+Z',
        role: 'undo',
      },
      {
        label: translation('edit.redo'),
        accelerator: 'Shift+Command+Z',
        role: 'redo',
      },
      {
        type: 'separator',
      },
      {
        label: translation('edit.cut'),
        accelerator: 'Command+X',
        role: 'cut',
      },
      {
        label: translation('edit.copy'),
        accelerator: 'Command+C',
        role: 'copy',
      },
      {
        label: translation('edit.paste'),
        accelerator: 'Command+V',
        role: 'paste',
      },
      {
        label: translation('edit.selectAll'),
        accelerator: 'Command+A',
        role: 'selectall',
      },
    ],
  },
  {
    label: translation('view'),
    submenu: [
      {
        label: translation('view.reload'),
        accelerator: 'Command+R',
        click: () => window.webContents.reload(),
      },
      {
        label: translation('view.toggleFullScreen'),
        accelerator: 'Ctrl+Command+F',
        click: () => window.setFullScreen(!window.isFullScreen()),
      },
      {
        label: translation('view.toggleDeveloperTools'),
        accelerator: 'Alt+Command+I',
        // @ts-ignore ts-migrate(2339) FIXME: Property 'toggleDevTools' does not exist on type '... Remove this comment to see the full error message
        click: () => window.toggleDevTools(),
      },
    ],
  },
  {
    label: translation('helpSupport'),
    submenu: (0, lodash_1.compact)([
      ...(0, submenuBuilders_1.buildKnownIssueFixesSubmenu)(
        actions,
        translations,
        translation
      ),
      {
        type: 'separator',
      },
      {
        label: translation('helpSupport.safetyTips'),
        click() {
          const safetyTipsLinkUrl = translation('helpSupport.safetyTipsUrl');
          electron_1.shell.openExternal(safetyTipsLinkUrl);
        },
      },
      {
        label: translation('helpSupport.supportRequest'),
        click() {
          const supportRequestLinkUrl = translation(
            'helpSupport.supportRequestUrl'
          );
          const supportUrl = (0, reporting_1.generateSupportRequestLink)(
            supportRequestLinkUrl,
            environment_1.environment,
            locale
          );
          electron_1.shell.openExternal(supportUrl);
        },
      },
      {
        label: translation('helpSupport.downloadLogs'),
        click() {
          // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
          control_ui_parts_1.showUiPartChannel.send(
            constants_1.NOTIFICATIONS.DOWNLOAD_LOGS,
            window
          );
        },
      },
      {
        type: 'separator',
      },
      {
        label: translation('helpSupport.daedalusDiagnostics'),
        accelerator: 'Command+D',
        click() {
          actions.openDaedalusDiagnosticsDialog();
        },
        enabled: isNavigationEnabled,
      },
    ]),
  },
];
exports.osxMenu = osxMenu;
//# sourceMappingURL=osx.js.map
