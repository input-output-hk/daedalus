// @flow
import type { App, BrowserWindow } from 'electron';
import { compact } from 'lodash';
import type { MenuActions } from './MenuActions.types';

export const osxMenu = (
  app: App,
  window: BrowserWindow,
  actions: MenuActions,
  isInSafeMode: boolean
) => (
  [{
    label: 'Daedalus',
    submenu: compact([{
      label: 'About',
      click() {
        actions.openAbout();
      },
    }, {
      label: 'Ada redemption',
      click() {
        actions.goToAdaRedemption();
      }
    }, {
      label: 'GPU safe mode',
      type: 'checkbox',
      checked: isInSafeMode,
      click() {
        isInSafeMode ?
          actions.restartWithoutSafeMode() :
          actions.restartInSafeMode();
      },
    }, {
      label: 'Network status',
      accelerator: 'Command+S',
      click() {
        actions.goToNetworkStatus();
      },
    }, {
      label: 'Quit',
      accelerator: 'Command+Q',
      click() {
        app.quit();
      }
    }])
  }, {
    label: 'Edit',
    submenu: [{
      label: 'Undo',
      accelerator: 'Command+Z',
      role: 'undo'
    }, {
      label: 'Redo',
      accelerator: 'Shift+Command+Z',
      role: 'redo'
    }, {
      type: 'separator'
    }, {
      label: 'Cut',
      accelerator: 'Command+X',
      role: 'cut'
    }, {
      label: 'Copy',
      accelerator: 'Command+C',
      role: 'copy'
    }, {
      label: 'Paste',
      accelerator: 'Command+V',
      role: 'paste'
    }, {
      label: 'Select All',
      accelerator: 'Command+A',
      role: 'selectall'
    }]
  }, {
    label: 'View',
    submenu: [
      {
        label: 'Reload',
        accelerator: 'Command+R',
        click: () => window.webContents.reload()
      },
      {
        label: 'Toggle Full Screen',
        accelerator: 'Ctrl+Command+F',
        click: () => window.setFullScreen(!window.isFullScreen())
      },
      {
        label: 'Toggle Developer Tools',
        accelerator: 'Alt+Command+I',
        click: () => window.toggleDevTools()
      }
    ]
  }]
);
