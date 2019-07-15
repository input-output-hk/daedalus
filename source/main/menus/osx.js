// @flow
import { compact } from 'lodash';
import { dialog, shell } from 'electron';
import type { App, BrowserWindow } from 'electron';
import type { MenuActions } from './MenuActions.types';
import { getTranslation } from '../utils/getTranslation';
import { environment } from '../environment';
import { showUiPartChannel } from '../ipc/control-ui-parts';
import { NOTIFICATIONS } from '../../common/ipc/constants';
import type { SupportRequests } from '../../common/types/support-requests.types';

const id = 'menu';
const { isInSafeMode } = environment;

export const osxMenu = (
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
      { type: 'separator' },
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
      { type: 'separator' },
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
      { type: 'separator' },
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
        click: () => window.toggleDevTools(),
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
          const gpuSafeModeDialogOptions = {
            buttons: [
              translation('helpSupport.gpuSafeModeDialogConfirm'),
              translation('helpSupport.gpuSafeModeDialogNo'),
              translation('helpSupport.gpuSafeModeDialogCancel'),
            ],
            type: 'warning',
            title: isInSafeMode
              ? translation('helpSupport.gpuSafeModeDialogTitle')
              : translation('helpSupport.nonGpuSafeModeDialogTitle'),
            message: isInSafeMode
              ? translation('helpSupport.gpuSafeModeDialogMessage')
              : translation('helpSupport.nonGpuSafeModeDialogMessage'),
            defaultId: 2,
            cancelId: 2,
          };
          dialog.showMessageBox(window, gpuSafeModeDialogOptions, buttonId => {
            if (buttonId === 0) {
              if (isInSafeMode) {
                actions.restartWithoutSafeMode();
              } else {
                actions.restartInSafeMode();
              }
            }
            item.checked = isInSafeMode;
          });
        },
      },
      { type: 'separator' },
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
        accelerator: 'Command+B',
        click() {
          actions.openBlockConsolidationStatusDialog();
        },
      },
      {
        label: translation('helpSupport.daedalusDiagnostics'),
        accelerator: 'Command+D',
        click() {
          actions.openDaedalusDiagnosticsDialog();
        },
      },
    ]),
  },
];
