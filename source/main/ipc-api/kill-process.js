// @flow
import { ipcMain, app } from 'electron';

export default () => {
  ipcMain.on('kill-process', () => {
    app.quit();
    // if (event.sender !== mainWindow.webContents) return;
    // TODO: fix this (are we ensuring that this message is only allowed from mainWindow?)
  });
};
