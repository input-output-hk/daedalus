export default (window, isDebug) => {
  return [{
    label: '&Daedalus',
    submenu: [{
      label: '&Close',
      accelerator: 'Ctrl+W',
      click() {
        window.close();
      }
    }]
  }, {
    label: '&View',
    submenu: (isDebug) ? [{
      label: '&Reload',
      accelerator: 'Ctrl+R',
      click() {
        window.webContents.reload();
      }
    }, {
      label: 'Toggle &Full Screen',
      accelerator: 'F11',
      click() {
        window.setFullScreen(!window.isFullScreen());
      }
    }, {
      label: 'Toggle &Developer Tools',
      accelerator: 'Alt+Ctrl+I',
      click() {
        window.toggleDevTools();
      }
    }] : [{
      label: 'Toggle &Full Screen',
      accelerator: 'F11',
      click() {
        window.setFullScreen(!window.isFullScreen());
      }
    }]
  }];
};
