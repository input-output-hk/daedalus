'use strict';
Object.defineProperty(exports, '__esModule', { value: true });
exports.winLinuxMenu = void 0;
const lodash_1 = require('lodash');
const electron_1 = require('electron');
const getTranslation_1 = require('../utils/getTranslation');
const environment_1 = require('../environment');
const constants_1 = require('../../common/ipc/constants');
const control_ui_parts_1 = require('../ipc/control-ui-parts');
const reporting_1 = require('../../common/utils/reporting');
const submenuBuilders_1 = require('./submenuBuilders');
const api_1 = require('../../common/ipc/api');
const id = 'menu';
const winLinuxMenu = (
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
        accelerator: 'Ctrl+T',
        click() {
          actions.openItnRewardsRedemptionDialog();
        },
        enabled: isNavigationEnabled,
      },
      {
        type: 'separator',
      },
      {
        label: translation('daedalus.close'),
        accelerator: 'Ctrl+W',
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
        accelerator: 'Ctrl+Z',
        role: 'undo',
      },
      {
        label: translation('edit.redo'),
        accelerator: 'Shift+Ctrl+Z',
        role: 'redo',
      },
      {
        type: 'separator',
      },
      {
        label: translation('edit.cut'),
        accelerator: 'Ctrl+X',
        role: 'cut',
      },
      {
        label: translation('edit.copy'),
        accelerator: 'Ctrl+C',
        role: 'copy',
      },
      {
        label: translation('edit.paste'),
        accelerator: 'Ctrl+V',
        role: 'paste',
      },
      {
        label: translation('edit.selectAll'),
        accelerator: 'Ctrl+A',
        role: 'selectall',
      },
    ],
  },
  {
    label: translation('view'),
    submenu: [
      {
        label: translation('view.reload'),
        accelerator: 'Ctrl+R',
        click() {
          window.webContents.reload();
        },
      },
      {
        type: 'separator',
      },
      {
        label: translation('daedalus.settings'),
        accelerator: 'Alt+S',
        click() {
          actions.openSettingsPage();
        },
        enabled: isNavigationEnabled,
      },
      {
        label: translation('daedalus.walletSettings'),
        accelerator: 'Alt+Ctrl+S',
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
      environment_1.environment.isWindows
        ? {
            label: translation('view.toggleFullScreen'),
            accelerator: 'F11',
            click() {
              window.setFullScreen(!window.isFullScreen());
            },
          }
        : {
            label: translation('view.toggleMaximumWindowSize'),
            accelerator: 'F11',
            click() {
              if (window.isMaximized()) {
                window.unmaximize();
              } else {
                window.maximize();
              }
            },
          },
      {
        label: translation('view.toggleDeveloperTools'),
        accelerator: 'Alt+Ctrl+I',
        click() {
          // @ts-ignore ts-migrate(2339) FIXME: Property 'toggleDevTools' does not exist on type '... Remove this comment to see the full error message
          window.toggleDevTools();
        },
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
      /* {
          label: translation('helpSupport.featureRequest'),
          click() {
            const featureRequestLinkUrl = translation(
              'helpSupport.featureRequestUrl'
            );
            shell.openExternal(featureRequestLinkUrl);
          },
        }, */
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
        accelerator: 'Ctrl+D',
        click() {
          actions.openDaedalusDiagnosticsDialog();
        },
        enabled: isNavigationEnabled,
      },
    ]),
  },
];
exports.winLinuxMenu = winLinuxMenu;
//# sourceMappingURL=win-linux.js.map
