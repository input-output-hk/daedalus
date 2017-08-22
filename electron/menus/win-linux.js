export default (app, window, mainEventHandler = null) => {
  return [{
    label: '&Daedalus',
    submenu: [{
      label: '&About',
      click() {
        mainEventHandler('open-about');
      }
    }, {
      label: '&Close',
      accelerator: 'Ctrl+W',
      click() {
        app.quit();
      }
    }]
  }, {
    label: '&View',
    submenu: [
      {
        label: '&Reload',
        accelerator: 'Ctrl+R',
        click() { window.webContents.reload(); }
      },
      {
        label: 'Toggle &Full Screen',
        accelerator: 'F11',
        click() { window.setFullScreen(!window.isFullScreen()); }
      },
      {
        label: 'Toggle &Developer Tools',
        accelerator: 'Alt+Ctrl+I',
        click() { window.toggleDevTools(); }
      }
    ]
  }];
};
