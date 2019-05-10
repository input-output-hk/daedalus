// @flow
import { compact} from 'lodash';
import { dialog, shell } from 'electron';
import type { App, BrowserWindow } from 'electron';
import type { MenuActions } from './MenuActions.types';
import { getTranslation } from '../utils/getTranslation';
import { environment } from '../environment';
import { getLocale } from '../utils/getLocale';
import { generateFileNameWithTimestamp } from '../../common/utils/files';

const localesFillForm = {
  'en-US': 'English',
  'ja-JP': 'Japanese',
};

const id = 'menu';

export const osxMenu = (app: App,
                        window: BrowserWindow,
                        actions: MenuActions,
                        isInSafeMode: boolean,
                        translations: {},
                        translation: Function = getTranslation(translations, id)) => [
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
        accelerator: 'Command+B',
        click() {
          actions.goBlockConsolidationStatus();
        },
      },
      {
        label: translation('daedalus.networkStatus'),
        accelerator: 'Command+S',
        click() {
          actions.openNetworkStatus();
        },
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
        click: () => window.toggleDevTools(),
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
            } else {
              item.checked = false;
            }
          });
        },
      },
      {
        label: translation('helpSupport.downloadLogs'),
        click() {
          const fileName = generateFileNameWithTimestamp();
          const destination = dialog.showSaveDialog({
            defaultPath: fileName,
          });
          if (destination) {
            // @todo
          }
        },
      },
      {
        label: translation('helpSupport.supportRequest'),
        click() {
          const supportRequestLinkUrl =
            translation('helpSupport.supportRequestUrl');
          const {
            version,
            apiVersion,
            network,
            build,
            installerVersion,
            os,
            buildNumber,
          } = environment;

          const locale = getLocale(network);

          const info = {
            frontendVersion: version,
            backendVersion: apiVersion,
            network: network === 'development' ? 'staging' : network,
            build,
            installerVersion,
            os,
            locale,
            product: `Daedalus wallet - ${network}`,
            supportLanguage: localesFillForm[locale],
            productVersion: `Daedalus ${version}+Cardano ${buildNumber}`,
          };
          const supportUrl = `${supportRequestLinkUrl}?${Object.entries(info)
            .map(
              ([key, val]: [string, any]) =>
                `${encodeURIComponent(key)}=${encodeURIComponent(val)}`
            )
            .join('&')}`;
          shell.openExternal(supportUrl);
        },
      },
      {
        label: translation('helpSupport.knownIssues'),
        click() {
          const faqLink =
            translation('helpSupport.knownIssuesUrl');
          shell.openExternal(faqLink);
        },
      },
    ]),
  },
];
