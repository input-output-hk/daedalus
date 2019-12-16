// @flow
import { compact } from 'lodash';
import { shell } from 'electron';
import type { App, BrowserWindow } from 'electron';
import type { MenuActions } from './MenuActions.types';
import { getTranslation } from '../utils/getTranslation';
import { environment } from '../environment';
import { NOTIFICATIONS } from '../../common/ipc/constants';
import { showUiPartChannel } from '../ipc/control-ui-parts';
import { generateSupportRequestLink } from '../../common/utils/reporting';

const id = 'menu';
const {
  isWindows,
  isBlankScreenFixActive,
  isIncentivizedTestnet,
} = environment;

export const winLinuxMenu = (
  app: App,
  window: BrowserWindow,
  actions: MenuActions,
  translations: {},
  locale: string,
  isUpdateAvailable: boolean,
  translation: Function = getTranslation(translations, id)
) => [
  {
    label: translation('daedalus'),
    submenu: compact([
      {
        label: translation('daedalus.about'),
        click() {
          actions.openAboutDialog();
        },
        enabled: !isUpdateAvailable,
      },
      {
        type: 'separator',
      },
      {
        label: translation('daedalus.settings'),
        accelerator: 'Ctrl+,',
        click() {
          actions.openSettingsPage();
        },
        enabled: !isUpdateAvailable,
      },
      {
        label: translation('daedalus.walletSettings'),
        accelerator: 'Ctrl+;',
        click() {
          actions.openWalletSettingsPage();
        },
        enabled: !isUpdateAvailable,
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
      isWindows
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
          window.toggleDevTools();
        },
      },
    ],
  },
  {
    label: translation('helpSupport'),
    submenu: compact([
      {
        label: translation('helpSupport.knownIssues'),
        click() {
          const faqLink = translation('helpSupport.knownIssuesUrl');
          shell.openExternal(faqLink);
        },
      },
      !isIncentivizedTestnet
        ? {
            label: translation('helpSupport.blankScreenFix'),
            type: 'checkbox',
            checked: isBlankScreenFixActive,
            click(item) {
              actions.toggleBlankScreenFix(item);
            },
          }
        : null,
      { type: 'separator' },
      {
        label: translation('helpSupport.safetyTips'),
        click() {
          const safetyTipsLinkUrl = translation('helpSupport.safetyTipsUrl');
          shell.openExternal(safetyTipsLinkUrl);
        },
      },
      {
        label: translation('helpSupport.featureRequest'),
        click() {
          const featureRequestLinkUrl = translation(
            'helpSupport.featureRequestUrl'
          );
          shell.openExternal(featureRequestLinkUrl);
        },
      },
      {
        label: translation('helpSupport.supportRequest'),
        click() {
          const supportRequestLinkUrl = translation(
            'helpSupport.supportRequestUrl'
          );
          const supportUrl = generateSupportRequestLink(
            supportRequestLinkUrl,
            environment,
            locale
          );
          shell.openExternal(supportUrl);
        },
      },
      {
        label: translation('helpSupport.downloadLogs'),
        click() {
          showUiPartChannel.send(NOTIFICATIONS.DOWNLOAD_LOGS, window);
        },
        enabled: !isUpdateAvailable,
      },
      { type: 'separator' },
      {
        label: translation('helpSupport.daedalusDiagnostics'),
        accelerator: 'Ctrl+D',
        click() {
          actions.openDaedalusDiagnosticsDialog();
        },
        enabled: !isUpdateAvailable,
      },
    ]),
  },
];
