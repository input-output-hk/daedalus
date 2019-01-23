import { compact } from 'lodash';

export const osxMenu = (app, window, {
  openAbout,
  goToAdaRedemption,
  goToNetworkStatus,
  restartInSafeMode,
  restartWithoutSafeMode,
  goBlockConsolidationStatus
}, isInSafeMode) => (
  [{
    label: 'Daedalus',
    submenu: compact([{
      label: 'About',
      click() {
        openAbout();
      },
    }, {
      label: 'Ada redemption',
      click() {
        goToAdaRedemption();
      }
    }, {
      label: 'GPU safe mode',
      type: 'checkbox',
      checked: isInSafeMode,
      click() {
        isInSafeMode ?
          restartWithoutSafeMode() :
          restartInSafeMode();
      },
    }, {
      label: 'Block-consolidation status',
      accelerator: 'Command+B',
      click() {
        goBlockConsolidationStatus();
      },
    }, {
      label: 'Network status',
      accelerator: 'Command+S',
      click() {
        goToNetworkStatus();
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
