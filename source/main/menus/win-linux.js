// @flow
import type { App, BrowserWindow } from 'electron';
import { compact } from 'lodash';
import { dialog } from 'electron';
import type { MenuActions } from './MenuActions.types';
import { getTranslation } from '../utils/getTranslation';
import { environment } from '../environment';

const id = 'menu';

export const winLinuxMenu = (
  app: App,
  window: BrowserWindow,
  actions: MenuActions,
  isInSafeMode: boolean,
  translations: {},
  translation: Function = getTranslation(translations, id)
) => [
  {
    label: translation('daedalus'),
    submenu: compact([
      {
        label: translation('daedalus.about'),
        click() {
          actions.openAbout();
        },
      },
      {
        label: translation('daedalus.adaRedemption'),
        click() {
          actions.goToAdaRedemption();
        },
      },
      {
        label: translation('daedalus.blockConsolidationStatus'),
        accelerator: 'Ctrl+B',
        click() {
          actions.goBlockConsolidationStatus();
        },
      },
      {
        label: translation('daedalus.networkStatus'),
        accelerator: 'Ctrl+S',
        click() {
          actions.openNetworkStatus();
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
      environment.isWindows
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
        label: translation('helpSupport.gpuSafeMode'),
        type: 'checkbox',
        checked: isInSafeMode,
        click(item) {
          const gpuSafeModeDialogOptions = {
            buttons: ['Yes', 'No'],
            title: isInSafeMode ? translation('helpSupport.gpuSafeModeDialogTitle') : translation('helpSupport.nonGpuSafeModeDialogTitle'),
            message: isInSafeMode ? translation('helpSupport.gpuSafeModeDialogMessage') : translation('helpSupport.nonGpuSafeModeDialogMessage'),
          };
          dialog.showMessageBox(window, gpuSafeModeDialogOptions, (buttonId) => {
            if (buttonId === 0) {
              if (isInSafeMode) {
                actions.restartWithoutSafeMode();
              } else {
                actions.restartInSafeMode();
              }
            } else {
              item.checked = false;
            }
          });
        },
      },
      {
        label: translation('helpSupport.downloadLogs'),
        click() {},
      },
      {
        label: translation('helpSupport.supportRequest'),
        click() {},
      },
      {
        label: translation('helpSupport.knownIssues'),
        click() {},
      },
    ]),
  },
];
