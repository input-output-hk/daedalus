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
const { isWindows, isBlankScreenFixActive } = environment;
export const winLinuxMenu = (
  app: App,
  window: BrowserWindow,
  actions: MenuActions,
  translations: {},
  locale: string,
  isNavigationEnabled: boolean,
  translation: (...args: Array<any>) => any = getTranslation(translations, id)
) => [
  {
    label: translation('daedalus'),
    submenu: compact([
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

        enabled: isNavigationEnabled,
      },
      {
        type: 'separator',
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
          // @ts-ignore ts-migrate(2339) FIXME: Property 'toggleDevTools' does not exist on type '... Remove this comment to see the full error message
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
      {
        label: translation('helpSupport.blankScreenFix'),
        type: 'checkbox',
        checked: isBlankScreenFixActive,

        click(item) {
          actions.toggleBlankScreenFix(item);
        },
      },
      {
        type: 'separator',
      },
      {
        label: translation('helpSupport.safetyTips'),

        click() {
          const safetyTipsLinkUrl = translation('helpSupport.safetyTipsUrl');
          shell.openExternal(safetyTipsLinkUrl);
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
          // @ts-ignore ts-migrate(2345) FIXME: Argument of type 'BrowserWindow' is not assignable... Remove this comment to see the full error message
          showUiPartChannel.send(NOTIFICATIONS.DOWNLOAD_LOGS, window);
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
