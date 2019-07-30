// @flow
import { compact } from 'lodash';
import { shell } from 'electron';
import type { App, BrowserWindow } from 'electron';
import type { MenuActions } from './MenuActions.types';
import { getTranslation } from '../utils/getTranslation';
import { environment } from '../environment';
import { NOTIFICATIONS } from '../../common/ipc/constants';
import { showUiPartChannel } from '../ipc/control-ui-parts';
import type { SupportRequests } from '../../common/types/support-requests.types';

const id = 'menu';
const { isWindows, isInSafeMode } = environment;

export const winLinuxMenu = (
  app: App,
  window: BrowserWindow,
  actions: MenuActions,
  translations: {},
  supportRequestData: SupportRequests,
  isNodeInSync: boolean,
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
      },
      {
        label: translation('daedalus.adaRedemption'),
        enabled: isNodeInSync,
        click() {
          actions.openAdaRedemptionScreen();
        },
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
      {
        label: translation('helpSupport.blankScreenFix'),
        type: 'checkbox',
        checked: isInSafeMode,
        click(item) {
          actions.toggleOnSafeMode(item);
        },
      },
      { type: 'separator' },
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
          const supportUrl = `${supportRequestLinkUrl}?${Object.entries(
            supportRequestData
          )
            .map(
              ([key, val]: [string, any]) =>
                `${encodeURIComponent(key)}=${encodeURIComponent(val)}`
            )
            .join('&')}`;
          shell.openExternal(supportUrl);
        },
      },
      {
        label: translation('helpSupport.downloadLogs'),
        click() {
          showUiPartChannel.send(NOTIFICATIONS.DOWNLOAD_LOGS, window);
        },
      },
      { type: 'separator' },
      {
        label: translation('helpSupport.blockConsolidationStatus'),
        accelerator: 'Ctrl+B',
        click() {
          actions.openBlockConsolidationStatusDialog();
        },
      },
      {
        label: translation('helpSupport.daedalusDiagnostics'),
        accelerator: 'Ctrl+D',
        click() {
          actions.openDaedalusDiagnosticsDialog();
        },
      },
    ]),
  },
];
